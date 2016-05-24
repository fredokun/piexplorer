

#include <iostream>
#include <chrono>
#include <cstdlib>

#include "graph.h"

using namespace std;
using namespace pisym;

int main(int argc, const char* argv[]) {

  if(argc != 2) {
    cerr << "Error: missing command line arguments" << endl;
    exit(EXIT_FAILURE);
  }

  const char* filename = argv[1];

  try {
    cout << "Load graph from file: '" << filename << "'" << endl;
    using namespace std::chrono;
    high_resolution_clock::time_point start_load_time = high_resolution_clock::now();
    Graph g = load_graph_from_file(filename, true);
    high_resolution_clock::time_point end_load_time = high_resolution_clock::now();

    duration<double> load_time = duration_cast<duration<double>>(end_load_time - start_load_time);
    cout << "(loading time = " << load_time.count() << "s)" << endl;
    cout << " ==> " << g.size() << " nodes in graph." << endl;
    cout << " ==> " << g.nb_blocks() << " blocks (prepartition by label)." << endl;

    //clog << g.lts_rep_with_blocks() << endl;

    // g.check_invariant();

    cout << "\nMinimization  ..." << endl;

    high_resolution_clock::time_point start_min_time = high_resolution_clock::now();
    bool minimized = minimize_graph(g);
    high_resolution_clock::time_point end_min_time = high_resolution_clock::now();
    duration<double> min_time = duration_cast<duration<double>>(end_min_time - start_min_time);
    cout << "(minimization time = " << min_time.count() << "s)" << endl;
    if(minimized) {
      cout << " ==> graph minimized to " << g.nb_blocks() << " nodes" << endl;
    } else {
      cout << " ==> graph is already minimized" << endl;
    }

    //cout << g.lts_rep_with_blocks();

    return 0;
  } catch(load_graph_exception &exc) {
    cerr << "Error while loading graph:" << endl;
    cerr << "  ==> file '" << filename << "' at line: " << exc.linepos << endl;
    cerr << "  ==> " << exc.message << endl;

    return EXIT_FAILURE;
  }
}
