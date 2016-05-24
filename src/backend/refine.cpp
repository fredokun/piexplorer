
#include <vector>
#include <set>
#include <iostream>
#include <cassert>

#include "utils.h"
#include "graph.h"


using namespace std;
using namespace pisym;

static bool refine_block(Graph &graph, size_t block_ref, const Block & splitter) {
  // clog << "----------------------------------" << endl;
  // clog << "[refine_block] block_ref = " << block_ref << "  splitter = " << splitter << endl;

  bool refined = false;

  Block & block = graph.get_block(block_ref);

  Block block_inter_splitter_pre;
  block_inter_splitter_pre.set_block_ref(graph.nb_blocks());

  for(const auto & splitter_pre_ref : splitter.get_pre_refs()) {
    Node & splitter_pre_node = graph.get_node(splitter_pre_ref);
      // clog << "Splitter pre-node #" << splitter_pre_node.node_index << " block ref = " << splitter_pre_node.get_block_ref() << endl;

    if(splitter_pre_node.get_block_ref() == block_ref) {
      refined = true;
      // this node is in the intersection
      block_inter_splitter_pre.add_node_ref(splitter_pre_ref);
      // XXX: this node should be removed from the block at the end
    } // else node not in the intersection => do nothing

  }



  // clog << "new intersection block = " << block_inter_splitter_pre << endl;
  // clog << "old block = " << block << endl;

  if(refined) {
    // Here, the block   Block <inter> Pre(Splitter)   has been formed

    if(block_inter_splitter_pre.size() == block.size()) {
      refined = false;  // total split : all nodes in the intersection
    } else {

      // change the block reference of all the nodes in the intersection
      for(auto & node_inter_ref : block_inter_splitter_pre.get_node_refs()) {
        Node & node_inter = graph.get_node(node_inter_ref);
        // clog << "update node #" << node_inter_ref << " to block ref #" << block_inter_splitter_pre.get_block_ref() << endl;
        node_inter.set_block_ref(block_inter_splitter_pre.get_block_ref());
      }

      const vector<size_t> & block_node_refs = block.get_node_refs();
      // clog << "[refine_block] block_node_refs = " << vector_to_string(block_node_refs) << endl;
      { size_t i = 0;
        while (i < block.size()) {
          const Node & node = graph.get_node(block_node_refs[i]);
          // clog << "Node #" << node.node_index << " (" << i << "-th of block has block ref = " << node.get_block_ref() << endl;
          if (node.get_block_ref() != block_ref) {
            block.remove_node_ref(i);
            // clog << "remove node #" << node.node_index << " in block" << endl;
          } else {
            i+=1;
          }
        }
      }

      // clog << "new old block = " << block << endl;

      // Then, we add the new block to the graph
      block_inter_splitter_pre.compute_pre_refs(graph);
      block.compute_pre_refs(graph);
      graph.add_block(block_inter_splitter_pre);
    }
  }

  //  if(refined) {
    // clog << "graph intersection block = " << graph.get_block(graph.nb_blocks()-1) << endl;
    // clog << "graph new old block = " << graph.get_block(block_ref) << endl;
  //  }

  // clog << graph.lts_rep_with_blocks() << endl;
  // graph.check_invariant();

  return refined;
}

static bool refine_step(Graph &graph) {
  vector<Block> splitters = graph.get_blocks_copy();
  bool refined = false;

  for(const auto & splitter : splitters) {
    for(size_t block_ref = 0; block_ref < graph.nb_blocks(); block_ref++) {
      refined |= refine_block(graph, block_ref, splitter);
    }
  }

  return refined;
}

bool pisym::minimize_graph(Graph &graph) {
  bool refined = false;
  size_t refine_count = 0;
  while (refine_step(graph)) {
    ++refine_count;
    clog << "Refinement #" << refine_count << " (" << graph.nb_blocks() << " blocks)" << endl;
    // clog << graph.lts_rep_with_blocks() << endl;
    refined = true;
  }

  return refined;

}
