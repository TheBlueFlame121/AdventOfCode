#include <algorithm>
#include <cstdio>
#include <cstdlib>
#include <fstream>
#include <iostream>
#include <map>
#include <ostream>
#include <sstream>
#include <string>
#include <vector>

using namespace std;

int main(int argc, char *argv[]) {
  string filename = "input";
  ifstream file(filename);
  string line;

  vector<int> a, b;
  int i, j;
  map<int, int> m;

  while (getline(file, line)) {
    stringstream ll(line);
    ll >> i >> j;
    a.push_back(i);
    b.push_back(j);
    m[j]++;
  }
  sort(a.begin(), a.end());
  sort(b.begin(), b.end());

  long long sum1 = 0, sum2 = 0;
  for (i = 0; i < a.size(); i++) {
    sum1 += abs(a[i] - b[i]);
  }

  cout << "Part 1: " << sum1 << endl;

  for (i = 0; i < a.size(); i++) {
    sum2 += a[i] * m[a[i]];
  }

  cout << "Part 2: " << sum2 << endl;
  return 0;
}
