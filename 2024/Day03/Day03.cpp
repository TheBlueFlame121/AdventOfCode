#include <algorithm>
#include <cstdio>
#include <cstdlib>
#include <fstream>
#include <functional>
#include <iostream>
#include <map>
#include <ostream>
#include <regex>
#include <sstream>
#include <string>
#include <vector>

using namespace std;

string extract_muls(string inp) {
  regex r("mul\\(\\d{1,3},\\d{1,3}\\)");
  smatch m;
  string out;
  string temp = inp;
  while (regex_search(temp, m, r)) {
    out.append(m.str());
    temp = m.suffix().str();
  }
  return out;
}

long long eval_muls(string inp) {
  regex r("(\\d{1,3}),(\\d{1,3})");
  smatch m;
  long long out = 0;
  while (regex_search(inp, m, r)) {
    out += stoi(m[1].str()) * stoi(m[2].str());
    inp = m.suffix().str();
  }
  return out;
}

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

  string input;
  while (file >> line) {
    input.append(line);
  }

  long long sum1 = eval_muls(extract_muls(input));
  cout << "Part 1: " << sum1 << endl;

  long long sum2 = 0;

  vector<string> split_by_dos = split(input, "do()");
  vector<vector<string>> full_split;
  vector<string> temp;
  for (auto v : split_by_dos) {
    temp = split(v, "don't()");
    sum2 += eval_muls(extract_muls(temp[0]));
  }

  cout << "Part 2: " << sum2 << endl;

  return 0;
}
