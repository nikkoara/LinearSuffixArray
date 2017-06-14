# Linear Suffix Array construction

AFAIK this is a straightforward implementation of the SA construction algorithm
described in:

 "Ko, Pang and Srinivas, Aluru, 
  Linear Time Construction of Suffix Arrays (2002).
  Computer Science Technical Reports. Paper 218."

I replaced the manual management of memory buffers, the signed integer logic and
limitation to the suffix array size, and tucked all code under its own
namespace.
