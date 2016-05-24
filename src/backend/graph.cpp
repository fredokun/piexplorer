
#include <fstream>
#include <string>
#include <stdexcept>
#include <iostream>
#include <sstream>
#include <set>

#include "utils.h"
#include "graph.h"

using namespace std;
using namespace pisym;

string pisym::SourcePos::lts_rep() const {
  string str{"@pos("};

  str += to_string(start_linepos) + ":" + to_string(start_colpos);
  str += "->";
  str += to_string(end_linepos) + ":" + to_string(end_colpos);

  return str + ")";
}


/////////////////////////
// Node implementation //
/////////////////////////

string pisym::Node::string_rep() const {
  string str{ "Node(" };

  str += "type=";
  switch(node_type) {
  case NodeType::STATE: str += "State"; break;
  case NodeType::TRANSITION: str += "Transition"; break;
  default:
    throw domain_error("Invalid node type");
  }

  str += ", name=\""; str += node_name;
  str += "\", index="; str += to_string(node_index);
  str += ", label=\""; str += label;

  str += "\")";

  return str;
}

ostream& pisym::operator<<(ostream& os, const Node& node) {
  return (os << node.string_rep());
}


//////////////////////////
// Block implementation //
//////////////////////////

void pisym::Block::compute_pre_refs(const Graph & graph) {
  pre_refs = set<node_index_t>{};

  for (const auto & node_ref : node_refs) {
    const Node & node = graph.get_node(node_ref);
    for(const auto & pre_ref : node.get_pre()) {
      pre_refs.insert(pre_ref);
    }
  }
}


string pisym::Block::string_rep() const {
  string str{"Block("};

  str += "index=" + to_string(block_ref);
  str += ",nodes=" + vector_to_string(node_refs);
  /*
  string delim = "";
  for(auto & elem : node_refs) {
    str += delim;
    delim = ",";
    str += to_string(elem);
  }
  str += "])";
  */

  return str;
}

ostream& pisym::operator<<(ostream& os, const Block& block) {
  return (os << block.string_rep());
}

//////////////////////////
// Graph implementation //
//////////////////////////

size_t pisym::Graph::add_state(string state_name, SourcePos source_pos, string label)
{
  size_t index = nodes.size();
  Node new_node = Node{NodeType::STATE, state_name, index, source_pos, label};
  //clog << "New state node: " << new_node << endl;
  nodes.push_back(new_node);
  return index;
}

size_t pisym::Graph::add_transition(string start_state_name, size_t start_state_index, string end_state_name, size_t end_state_index, string label)
{

  Node & start_node = nodes[start_state_index];
  Node & end_node = nodes[end_state_index];

  size_t trans_node_index = nodes.size();

  Node trans_node = Node(NodeType::TRANSITION,
                         string("T_") + start_state_name + "_" + end_state_name
                         , trans_node_index, start_node.source_pos, label);

  //clog << "New transition node: " << trans_node << endl;

  start_node.add_successor(trans_node);

  trans_node.add_predecessor(start_node);
  trans_node.add_successor(end_node);
  end_node.add_predecessor(trans_node);

  nodes.push_back(trans_node);

  return nodes.size() - 1;
}



void pisym::Graph::check_invariant() const {
  // check nodes
  for(size_t i = 0; i < nodes.size(); i++) {
    const Node & node = nodes[i];
    if(node.node_index != i) {
      throw invariant_error("Wrong node index in graph");
    }

    switch(node.node_type) {
    case NodeType::STATE: {
      // nothing to do ?
    } break;
    case NodeType::TRANSITION: {
      if(node.get_pre().size() != 1) {
        throw invariant_error("Node #" + to_string(node.node_index) + " has not exactly one predecessor.");
      }
      if(node.get_post().size() != 1) {
        throw invariant_error("Node #" + to_string(node.node_index) + " has not exactly one successor.");
      } break;
    }
    }

    for(auto & pre_ref  : node.get_pre()) {
      const Node & pre_node = nodes[pre_ref];
      bool found_node = false;
      for(auto & pre_post_ref : pre_node.get_post()) {
        if(pre_post_ref == node.node_index) {
          if(!found_node) {
            found_node = true;
          } else {
            throw invariant_error("Node #" + to_string(node.node_index) + "' is twice succesor in predecessor #" + to_string(pre_node.node_index));
          }
        }
      }

      if(!found_node) {
        throw invariant_error("Node #" + to_string(node.node_index) + "' is not successor of its predecessor #" + to_string(pre_node.node_index));
      }

    }

    for(auto & post_ref  : node.get_post()) {
      const Node & post_node = nodes[post_ref];
      bool found_node = false;
      for(auto & post_pre_ref : post_node.get_pre()) {
        if(post_pre_ref == node.node_index) {
          if(!found_node) {
            found_node = true;
          } else {
            throw invariant_error("Node #" + to_string(node.node_index) + "' is twice predecessor in successor #" + to_string(post_node.node_index));
          }
        }
      }

      if(!found_node) {
        throw invariant_error("Node #" + to_string(node.node_index) + "' is not successor of its predecessor #" + to_string(post_node.node_index));
      }

    }


  }


  // check blocks

  for(size_t block_ref = 0 ; block_ref < blocks.size(); block_ref++) {
    const Block & block = blocks[block_ref];
    if(block.get_block_ref() != block_ref) {
      throw invariant_error("Block #" + to_string(block_ref) + " has wrong ref: " + to_string(block.get_block_ref()));
    }

    for(auto & node_ref : block.get_node_refs()) {
      const Node & node = nodes[node_ref];
      if(node.get_block_ref() != block_ref) {
        throw invariant_error("In block #" + to_string(block_ref) + " Node #" + to_string(node.node_index) + " has wrong block reference: " + to_string(node.get_block_ref()));
      }
    }

  }
}


string pisym::Graph::lts_rep_with_blocks() const {
  stringstream buf_graph;
  stringstream buf_trans;

  buf_graph << "digraph {" << endl;

  for(auto &block : blocks) {
    bool block_generated = false;
    for(auto &node_ref : block.get_node_refs()) {
      const Node & node = nodes[node_ref];
      switch(node.node_type) {
      case NodeType::STATE:
        if(!block_generated) {
          buf_graph << "  subgraph cluster_" << block.get_block_ref() << " {" << endl;
          block_generated = true;
        }
        buf_graph << "    " << state_repr(node.node_name, node.source_pos) << endl;
        break;
      case NodeType::TRANSITION:
        buf_trans << "  " << transition_repr(node.get_pre()[0], node.get_post()[0], node.label) << endl;
        break;
      default:
        throw domain_error("Invalid node type");
      }
    }

    if(block_generated) {
      buf_graph << "  }" << endl;
    }

  }

  buf_graph << endl << buf_trans.str();

  buf_graph << "}" << endl;

  return buf_graph.str();
}
