#include <algorithm>
#include <climits>
#include <fstream>
#include <iostream>
#include <map>
#include <queue>
#include <set>
#include <string>
#include <vector>

using namespace std;
using ll = long long;

vector<string> split(const string s, const string delimiter) {
  vector<string> tokens;
  size_t last = 0;
  size_t next = 0;
  while ((next = s.find(delimiter, last)) != string::npos) {
    tokens.push_back(s.substr(last, next - last));
    last = next + delimiter.length();
  }
  tokens.push_back(s.substr(last));
  return tokens;
}

map<string, bool> cache1;
map<string, ll> cache2;

bool can_obtain(string towel, set<string> patterns, int min, int max) {
  if (towel.size() == 0) {
    return true;
  }
  if (cache1.contains(towel)) {
    return cache1[towel];
  }
  for (int i = min; i <= max; i++) {
    if (patterns.contains(towel.substr(0, i)) &&
        can_obtain(towel.substr(i), patterns, min, max)) {
      cache1[towel] = true;
      return true;
    }
  }
  cache1[towel] = false;
  return false;
}

ll num_possibilities(string towel, set<string> patterns, int low, int max) {
  if (towel.size() == 0) {
    return 1;
  }
  if (cache2.contains(towel)) {
    return cache2[towel];
  }
  ll count = 0;
  for (int i = low; i <= min((size_t)max, towel.size()); i++) {
    if (patterns.contains(towel.substr(0, i))) {
      count += num_possibilities(towel.substr(i), patterns, low, max);
    }
  }
  cache2[towel] = count;
  return count;
}

int main(int argc, char *argv[]) {
  ifstream file("input");
  string line;
  getline(file, line);

  vector<string> temp = split(line, ", ");
  sort(temp.begin(), temp.end(),
       [](const std::string &s1, const std::string &s2) {
         return s1.length() < s2.length();
       });
  int min = temp[0].size();
  int max = temp[temp.size() - 1].size();
  set<string> patterns(temp.begin(), temp.end());

  vector<string> towels;
  getline(file, line);
  while (getline(file, line)) {
    towels.push_back(line);
  }

  ll sum1 = 0;
  for (auto t : towels) {
    // cout << t << " " << can_obtain(t, patterns, min, max) << endl;
    if (can_obtain(t, patterns, min, max)) {
      sum1++;
    }
  }
  cout << "Part 1: " << sum1 << endl;

  ll sum2 = 0;
  for (auto t : towels) {
    // cout << t << " " << num_possibilities(t, patterns, min, max) << endl;
    sum2 += num_possibilities(t, patterns, min, max);
  }
  cout << "Part 2: " << sum2 << endl;

  return 0;
}
