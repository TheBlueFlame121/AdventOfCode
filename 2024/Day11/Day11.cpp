#include <algorithm>
#include <fstream>
#include <iostream>
#include <map>
#include <set>
#include <string>
#include <unordered_map>
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

long long part1(vector<int> input, int iterations) {
  vector<long long> temp1, temp2;
  temp1.insert(temp1.end(), input.begin(), input.end());
  for (int i = 0; i < iterations; i++) {
    temp2.clear();
    for (auto val : temp1) {
      if (val == 0) {
        temp2.push_back(1);
      } else if (to_string(val).length() % 2 == 0) {
        // even size stuff
        string sval = to_string(val);
        int vallen = sval.length();
        temp2.push_back(stoll(sval.substr(0, vallen / 2)));
        temp2.push_back(stoll(sval.substr(vallen / 2)));
      } else {
        temp2.push_back(val * 2024);
      }
    }
    temp1 = temp2;
  }
  return temp1.size();
}

string key(long long val, int steps) {
  string temp = to_string(val);
  temp.append(",");
  temp.append(to_string(steps));
  return temp;
}

unordered_map<string, long long> Cache;
long long part2(long long val, int steps_left) {
  if (steps_left == 0) {
    return 1;
  }
  string k = key(val, steps_left);
  if (Cache.contains(k)) {
    return Cache[k];
  }
  if (val == 0) {

    Cache[k] = part2(1, steps_left - 1);
    return Cache[k];
  } else if (to_string(val).length() % 2 == 0) {
    // even size stuff
    string sval = to_string(val);
    int vallen = sval.length();
    Cache[k] = part2(stoll(sval.substr(0, vallen / 2)), steps_left - 1) +
               part2(stoll(sval.substr(vallen / 2)), steps_left - 1);
    return Cache[k];
  } else {
    Cache[k] = part2(val * 2024, steps_left - 1);
    return Cache[k];
  }
}

int main(int argc, char *argv[]) {
  string filename = "input";
  ifstream file(filename);
  string line;

  vector<int> input;
  while (getline(file, line)) {
    for (auto val : split(line, " ")) {
      input.push_back(stoi(val));
    }
  }

  long long sum1 = part1(input, 25);
  cout << "Part 1: " << sum1 << endl;

  long long sum2 = 0;
  for (auto val : input) {
    sum2 += part2(val, 75);
  }
  cout << "Part 2: " << sum2 << endl;

  return 0;
}
