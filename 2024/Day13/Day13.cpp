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

template <class T> ostream &operator<<(ostream &os, const vector<T> &p) {
  for (auto i : p) {
    os << i << " ";
  }
  return os << endl;
}

pair<ll, ll> solve(tuple<ll, ll, ll> eq1, tuple<ll, ll, ll> eq2) {
  ll a, b, c, d, e, f;
  tie(a, b, c) = eq1;
  tie(d, e, f) = eq2;
  ll denom = a * e - b * d;
  if (denom == 0)
    return {-1, -1};
  ll xnum = (c * e - b * f);
  ll ynum = (a * f - c * d);
  ll x = xnum / denom;
  ll y = ynum / denom;
  if (x * denom != xnum || y * denom != ynum)
    return {-1, -1};
  return {x, y};
}

int main(int argc, char *argv[]) {
  string filename = "input";
  ifstream file(filename);
  string line;

  vector<pair<tuple<ll, ll, ll>, tuple<ll, ll, ll>>> input;
  while (getline(file, line)) {
    if (line == "") {
      continue;
    }
    vector<string> temp = split(line.substr(10), ", ");
    vector<long long> eq1 = {stoll(temp[0].substr(2)),
                             stoll(temp[1].substr(2))};
    // cout << line.substr(10);
    // cout << " " << eq1[0] << " " << eq1[1] << '\n';

    getline(file, line);
    temp = split(line.substr(10), ", ");
    vector<long long> eq2 = {stoll(temp[0].substr(2)),
                             stoll(temp[1].substr(2))};
    // cout << line.substr(10);
    // cout << " " << eq2[0] << " " << eq2[1] << '\n';

    getline(file, line);
    temp = split(line.substr(7), ", ");
    vector<long long> eq3 = {stoll(temp[0].substr(2)),
                             stoll(temp[1].substr(2))};
    // cout << line.substr(7);
    // cout << " " << eq3[0] << " " << eq3[1] << '\n';

    input.push_back({{eq1[0], eq2[0], eq3[0]}, {eq1[1], eq2[1], eq3[1]}});
  }

  // cout << input.size() << '\n';

  long long sum1 = 0;
  for (auto eqs : input) {
    pair<ll, ll> soln;
    soln = solve(eqs.first, eqs.second);
    // cout << soln.first << " " << soln.second << '\n';
    if (soln.first < 0 || soln.second < 0 || soln.first > 100 ||
        soln.second > 100)
      continue;
    sum1 += soln.first * 3 + soln.second;
  }
  cout << "Part 1: " << sum1 << endl;

  long long sum2 = 0;
  for (auto eqs : input) {
    tuple<ll, ll, ll> eq1 = eqs.first, eq2 = eqs.second;
    pair<ll, ll> soln;
    eq1 = {get<0>(eq1), get<1>(eq1), get<2>(eq1) + 10000000000000};
    eq2 = {get<0>(eq2), get<1>(eq2), get<2>(eq2) + 10000000000000};
    soln = solve(eq1, eq2);
    if (soln.first < 0 || soln.second < 0)
      continue;
    sum2 += soln.first * 3 + soln.second;
  }
  cout << "Part 2: " << sum2 << endl;

  return 0;
}
