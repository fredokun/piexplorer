
#include <fstream>
#include <string>
#include <map>
#include <regex>

#include <iostream>

#include "graph.h"

using namespace std;
using namespace pisym;

Graph pisym::load_graph_from_file(const char* filename, bool prepartition_by_label) {
  ifstream infile(filename, ifstream::in);
  return load_graph(infile, prepartition_by_label);
}

Graph pisym::load_graph(ifstream & infile, bool prepartition_by_label) {

  string line;

  // parse header
  getline(infile, line);

  int linepos = 1;

  if(line != "/* Graph generated by *pisym* tool */") {
    throw load_graph_exception(linepos, "Wrong header for graph"); 
  }

  ++linepos;

  // parse source line
  regex src_re { "// @src\\(\"([^\"]+)\"\\)" };
  smatch matches;

  getline(infile, line);

  if (!regex_match(line, matches, src_re) ) {
    throw load_graph_exception(linepos, "No source indicated for graph");
  }

  string src = matches[1];

  Graph graph(src);

  ++linepos;

  // parse the digraph line
  getline(infile, line);

  if(line != "digraph {") {
    throw load_graph_exception(linepos, "Missing 'digraph' statement in graph"); 
  }

  // parse content

  map<string, size_t> state_idx_map;
  map<string, Block> trans_blocks_map;
  Block states_block;

  while (getline(infile, line)) {
    ++linepos;

    if(line.size() > 0 && line[0] == '}') {
      break;
    }

    // cout << "line = " << line << endl;

    regex state_re {"  (\\w+) // @pos\\((\\d+):(\\d+)->(\\d+):(\\d+)\\)"};
    regex trans_re {"  (\\w+) -> (\\w+) \\[label=\"([^\"]+)\"\\];"};

    if(regex_match(line, matches, state_re)) {
      string state_name = matches[1];

      int start_linepos, start_colpos, end_linepos, end_colpos;
      try {
        start_linepos = stoi(matches[2]);
        start_colpos = stoi(matches[3]);
        end_linepos = stoi(matches[4]);
        end_colpos = stoi(matches[5]);
      }
      catch (const invalid_argument& ia) {
        throw load_graph_exception(linepos, ia.what());
      }

      SourcePos sourcePos(start_linepos, start_colpos, end_linepos, end_colpos);

      size_t state_index = graph.add_state(state_name, sourcePos, string(""));
      state_idx_map[state_name] = state_index;

      if(prepartition_by_label) {
        states_block.add_node_ref(state_index);
      }

    } else if(regex_match(line, matches, trans_re)) {
      string start_state_name = matches[1];
      size_t start_state_index = state_idx_map[start_state_name];

      string end_state_name = matches[2];
      size_t end_state_index = state_idx_map[end_state_name];

      string trans_label = matches[3];

      size_t trans_index = graph.add_transition(start_state_name, start_state_index, end_state_name, end_state_index, trans_label);

      auto finder = trans_blocks_map.find(trans_label);
      if (finder != trans_blocks_map.end()) {
        (*finder).second.add_node_ref(trans_index);
      } else {
        Block trans_block;
        trans_block.add_node_ref(trans_index);
        trans_blocks_map[trans_label] = trans_block;
      }
    } else {
      throw load_graph_exception(linepos, "Cannot parse state or transition");
    }


  } // while


  // add the computed blocks to the graph
  if(prepartition_by_label) {
    // clog << "[load_graph] states block = " << states_block.string_rep() << endl;
    states_block.compute_pre_refs(graph);
    graph.add_block(states_block);

    for(auto &current : trans_blocks_map) {
      // clog << "[load_graph] transition block for label '" << current.first << "': " << current.second.string_rep() << endl;
      auto & trans_block = current.second;

      trans_block.compute_pre_refs(graph);
      size_t block_ref = graph.add_block(trans_block);
      for(auto & node_ref : trans_block.get_node_refs()) {
        Node & trans_node = graph.get_node(node_ref);
        trans_node.set_block_ref(block_ref);
      }
    }
  }

  return graph;

}
