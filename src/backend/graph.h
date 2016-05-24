
#ifndef GRAPH_H
#define GRAPH_H

#include <fstream>
#include <vector>
#include <string>
#include <set>

//#include <cstdlib>

namespace pisym {

  using namespace std;

  class SourcePos {
  public:
    const int start_linepos;
    const int start_colpos;
    const int end_linepos;
    const int end_colpos;

    explicit SourcePos(int start_linepos, int start_colpos, int end_linepos, int end_colpos)
      : start_linepos(start_linepos), start_colpos(start_colpos), end_linepos(end_linepos), end_colpos(end_colpos)
    {}

    string lts_rep() const;

  };

  enum class NodeType {
    STATE, TRANSITION
      };

  class Block;

  using block_index_t = vector<Block>::size_type;

  class Node;

  using node_index_t = vector<Node>::size_type;

  class Graph;

  class Node {
  public:
    const NodeType node_type;
    const string node_name;
    const node_index_t node_index;
    const SourcePos source_pos;
    const string label;
  private:
    vector<node_index_t> pre;
    vector<node_index_t> post;
    block_index_t block_index;

    int delta_before;
    int delta_after;   // for the hopcroft optimization

  public:
    explicit Node(const NodeType node_type, const string name, const node_index_t index, const SourcePos source_pos, const string label)
      : node_type(node_type), node_name(name), node_index(index), source_pos(source_pos), label(label),
      pre { vector<node_index_t> {} }, post { vector<node_index_t> {} }, block_index {0} {}

    const vector<node_index_t> & get_pre() const
    {
      return pre;
    }

    const vector<node_index_t> & get_post() const
    {
      return post;
    }

    void add_successor(const Node &succ)
    {
      post.push_back(succ.node_index);
    }

    void add_predecessor(const Node &prec)
    {
      pre.push_back(prec.node_index);
    }

    void set_block_ref(block_index_t block_ref) {
      block_index = block_ref;
    }

    block_index_t get_block_ref() const {
      return block_index;
    }

    string string_rep() const;

  };

  ostream& operator<<(ostream& os, const Node& node);

  class Block {
  private:
    block_index_t block_ref;
    vector<node_index_t> node_refs;
    set<node_index_t> pre_refs;

  public:
    explicit Block() : block_ref(0), node_refs(vector<node_index_t>{}) {}

    block_index_t get_block_ref() const {
      return block_ref;
    }

    const vector<node_index_t> & get_node_refs() const {
      return node_refs;
    }

    const set<node_index_t> & get_pre_refs() const {
      return pre_refs;
    }

    void add_node_ref(block_index_t node_index) {
      node_refs.push_back(node_index);
    }

    void set_block_ref(block_index_t ref) {
      block_ref = ref;
    }

    node_index_t get_node_ref(size_t i) const {
      return node_refs[i];
    }

    void remove_node_ref(node_index_t i) {
      if(i < node_refs.size() - 1) {
        node_refs[i] = node_refs.back();
      }
      node_refs.pop_back();
    }

    void compute_pre_refs(const Graph & graph);

    size_t size() const { return node_refs.size(); }

    string string_rep() const;

  };

  ostream& operator<<(ostream& os, const Block& block);

  class Graph {
  private:
    string source;
    vector<Node> nodes;
    vector<Block> blocks;
  public:
  Graph(string source) : source(source), nodes(vector<Node>()), blocks(vector<Block>()) {}

    size_t size() { return nodes.size(); }

    size_t nb_blocks() { return blocks.size(); }

    size_t add_state(string state_name, SourcePos source_pos, string label);

    size_t add_transition(string start_state_name, size_t start_state_index, string end_state_name, size_t end_state_index, string label);

    vector<Block> get_blocks_copy() { return blocks; }

    size_t add_block(Block block) {
      size_t ref = blocks.size();
      block.set_block_ref(ref);
      blocks.push_back(block);
      return ref;
    }

    Node & get_node(size_t node_ref) {
       return nodes[node_ref];
    }

    const Node & get_node(size_t node_ref) const {
       return nodes[node_ref];
    }

    Block & get_block(size_t block_ref) {
      return blocks[block_ref];
    }

    const Block & get_block(size_t block_ref) const {
      return blocks[block_ref];
    }

    void check_invariant() const;

    string transition_repr(size_t start_ref, size_t end_ref, const string & label) const {
      return string{""} + nodes[start_ref].node_name + " -> " + nodes[end_ref].node_name + " [label=\"" + label + "\"];";
    }

    string state_repr(const string & node_name, const SourcePos & source_pos) const {
      return string{""} + node_name + " // " + source_pos.lts_rep();
    }

    string lts_rep_with_blocks() const;

  };


  class load_graph_exception {
  public:
    const int linepos;
    const string message;

  load_graph_exception(const int pos, const string msg) : linepos(pos), message(msg) {}

  };

  Graph load_graph_from_file(const char * filename, bool prepartition_by_label=false);
  Graph load_graph(ifstream & infile, bool prepartition_by_label=false);

  bool minimize_graph(Graph &graph);

}


#endif // GRAPH_H
