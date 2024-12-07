#include <algorithm>
#include <cstdio>
#include <cstdlib>
#include <fstream>
#include <functional>
#include <iostream>
#include <map>
#include <ostream>
#include <sstream>
#include <string>
#include <vector>

using namespace std;

bool is_safe(vector<int> levels) {
  vector<int> sa(levels), ra(levels);
  sort(sa.begin(), sa.end());
  sort(ra.begin(), ra.end(), greater<>());
  if (levels != sa && levels != ra) {
    return false;
  }
  for (int i = 0; i < levels.size() - 1; i++) {
    int diff = abs(levels[i] - levels[i + 1]);
    if (diff > 3 || diff < 1) {
      return false;
    }
  }
  return true;
}

int main(int argc, char *argv[]) {
  string filename = "input";
  ifstream file(filename);
  string line;

  vector<vector<int>> a;
  int i, j;

  while (getline(file, line)) {
    stringstream ll(line);
    vector<int> temp;
    while (ll >> i) {
      temp.push_back(i);
    }
    a.push_back(temp);
  }

  long long sum1 = 0, sum2 = 0;
  for (i = 0; i < a.size(); i++) {
    if (is_safe(a[i])) {
      sum1++;
    }
  }

  cout << "Part 1: " << sum1 << endl;

  for (i = 0; i < a.size(); i++) {
    if (is_safe(a[i])) {
      sum2++;
    } else {
      for (int j = 0; j < a[i].size(); j++) {
        vector<int> temp(a[i]);
        temp.erase(temp.begin() + j);
        if (is_safe(temp)) {
          sum2++;
          break;
        }
      }
    }
  }

  cout << "Part 2: " << sum2 << endl;

  return 0;
}
