# -*- mode: Makefile; -*-

OSNAME := $(shell uname)

ifeq ($(OSNAME),Darwin)
include ./Makefile.darwin
endif

ifeq ($(OSNAME),Linux)
include ./Makefile.linux
endif

########################################################################

BENCHMARK_CPPFLAGS = -I/usr/local/include
BENCHMARK_LDFLAGS = -L/usr/local/lib

BENCHMARK_LIBS = -lbenchmark

########################################################################

CXXFLAGS = $(MTFLAGS) -O3 -g -std=c++1z 

CPPFLAGS = -I. -I.. $(BENCHMARK_CPPFLAGS)
LDFLAGS  = $(MTFLAGS) $(BENCHMARK_LDFLAGS)

LIBS = $(BENCHMARK_LIBS)

DEPENDDIR = ./.deps
DEPENDFLAGS = -M

SRCS := $(wildcard *.cpp)
OBJS := $(patsubst %.cpp,%.o,$(SRCS))

TARGETS = ref benchmark test

########################################################################

all: $(TARGETS)

DEPS = $(patsubst %.o,$(DEPENDDIR)/%.d,$(OBJS))
-include $(DEPS)

$(DEPENDDIR)/%.d: %.cpp $(DEPENDDIR)
	$(CXX) $(CXXFLAGS) $(CPPFLAGS) $(DEPENDFLAGS) $< >$@

$(DEPENDDIR):
	@[ ! -d $(DEPENDDIR) ] && mkdir -p $(DEPENDDIR)

%: %.o sa.o
	$(CXX) $(LDFLAGS) -o $@ $^ $(LIBS)

%.o: %.cpp
	$(CXX) $(CXXFLAGS) $(CPPFLAGS) -c $< -o $@

%: %.cpp

.PHONY: clean

clean:
	rm -rf $(OBJS) $(TARGETS) $(DEPENDDIR) *.gcda *.gcno
