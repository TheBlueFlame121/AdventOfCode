#include <algorithm>
#include <fstream>
#include <iostream>
#include <map>
#include <string>
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

int main(int argc, char *argv[]) {
  string filename = "input";
  ifstream file(filename);
  string line;

  vector<string> rules, input;
  while (getline(file, line)) {
    if (line.empty()) {
      break;
    }
    rules.push_back(line);
  }
  while (file >> line) {
    input.push_back(line);
  }

  map<int, vector<int>> rulemap;
  for (auto r : rules) {
    vector<string> ints = split(r, "|");
    rulemap[stoi(ints[0])].push_back(stoi(ints[1]));
  }
  vector<vector<int>> pages;
  {
    for (auto i : input) {
      vector<int> temp;
      for (auto val : split(i, ",")) {
        temp.push_back(stoi(val));
      }
      pages.push_back(temp);
    }
  }

  auto compare = [&rulemap](int a, int b) {
    auto vec = rulemap[a];
    if (std::find(vec.begin(), vec.end(), b) != vec.end()) {
      return true;
    }
    return false;
  };

  long long sum1 = 0;
  for (auto page : pages) {
    if (is_sorted(page.begin(), page.end(), compare)) {
      int mid = page[(page.size() / 2)];
      sum1 += mid;
    }
  }
  cout << "Part 1: " << sum1 << endl;

  long long sum2 = 0;
  for (auto page : pages) {
    if (is_sorted(page.begin(), page.end(), compare)) {
      continue;
    } else {
      sort(page.begin(), page.end(), compare);
      int mid = page[(page.size() / 2)];
      sum2 += mid;
    }
  }
  cout << "Part 2: " << sum2 << endl;

  return 0;
}
