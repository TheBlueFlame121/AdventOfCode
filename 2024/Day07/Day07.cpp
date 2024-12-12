#include <algorithm>
#include <bitset>
#include <cmath>
#include <fstream>
#include <iostream>
#include <map>
#include <set>
#include <string>
#include <unordered_set>
#include <vector>

using namespace std;

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

long long part1(vector<pair<long long, vector<long long>>> data) {
  long long sum = 0;
  for (auto d : data) {
    long long val;
    vector<long long> ints;
    tie(val, ints) = d;
    int found = 0;
    for (int i = 0; i <= pow(2, ints.size() - 1) - 1; i++) {
      int b = i;
      long long rs = ints[0];
      for (int bits = 0; bits < ints.size() - 1; bits++) {
        if (b & 1) {
          rs += ints[bits + 1];
        } else {
          rs *= ints[bits + 1];
        }
        b >>= 1;
      }
      if (rs == val) {
        found = 1;
        break;
      }
    }
    if (found) {
      sum += val;
    }
  }
  return sum;
}

long long part2(vector<pair<long long, vector<long long>>> data) {
  long long sum = 0;
  for (auto d : data) {
    long long val;
    vector<long long> ints;
    tie(val, ints) = d;
    int found = 0;
    for (int i = 0; i <= pow(3, ints.size() - 1) - 1; i++) {
      int b = i;
      long long rs = ints[0];
      for (int bits = 0; bits < ints.size() - 1; bits++) {
        int trit = b % 3;
        if (trit == 0) {
          rs += ints[bits + 1];
        } else if (trit == 1) {
          rs *= ints[bits + 1];
        } else {
          rs = stoll(to_string(rs).append(to_string(ints[bits + 1])));
        }
        b /= 3;
      }
      if (rs == val) {
        found = 1;
        break;
      }
    }
    if (found) {
      sum += val;
    }
  }
  return sum;
}

int main(int argc, char *argv[]) {
  string filename = "input";
  ifstream file(filename);
  string line;

  vector<string> input;
  while (getline(file, line)) {
    input.push_back(line);
  }

  vector<pair<long long, vector<long long>>> data;
  for (auto l : input) {
    vector<string> temp = split(l, ": ");
    vector<long long> vals;
    for (auto t : split(temp[1], " ")) {
      vals.push_back(stoll(t));
    }
    data.push_back({stoll(temp[0]), vals});
  }

  long long sum1 = 0;
  sum1 = part1(data);
  cout << "Part 1: " << sum1 << endl;

  long long sum2 = 0;
  sum2 = part2(data);
  cout << "Part 2: " << sum2 << endl;

  return 0;
}
