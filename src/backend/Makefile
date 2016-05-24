
CXX= g++ #g++-5 #clang++-3.7
CXXFLAGS= -O3 -g -Wall -std=c++11

LDFLAGS=

AR= ar
ARFLAGS= rvs

all: test_loadgraph

utils.o: utils.cpp utils.h
	$(CXX) -c $(CXXFLAGS) $< -o $@

graph.o: graph.cpp graph.h
	$(CXX) -c $(CXXFLAGS) $< -o $@

loadgraph.o: loadgraph.cpp graph.h
	$(CXX) -c $(CXXFLAGS) $< -o $@

refine.o: refine.cpp graph.h
	$(CXX) -c $(CXXFLAGS) $< -o $@

test_loadgraph: graph.o loadgraph.o refine.o utils.o test_loadgraph.cpp
	$(CXX) $(CXXFLAGS) $(LDFLAGS) $^ -o $@


clean:
	rm -f *~
	rm -f utils.o graph.o loadgraph.o refine.o

cleanall: clean
	rm -f test_loadgraph

