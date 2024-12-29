#include <algorithm>
#include <cinttypes>
#include <climits>
#include <fstream>
#include <iostream>
#include <limits>
#include <map>
#include <numeric>
#include <queue>
#include <ranges>
#include <set>
#include <sstream>
#include <string>
#include <tuple>
#include <vector>

using namespace std;
using ll = long long;

template <class T> ostream &operator<<(ostream &os, const vector<T> &p) {
  if (is_same<T, string>::value) {
    for (auto i : p) {
      os << i << '\n';
    }
  } else {
    for (auto i : p) {
      os << i << " ";
    }
    os << endl;
  }
  return os;
}

void evolve_inplace(ll &in) {
  in = (in ^ (in * 64)) % 16777216;
  in = (in ^ (in / 32)) % 16777216;
  in = (in ^ (in * 2048)) % 16777216;
}

ll evolve(ll in) {
  ll temp = (in ^ (in * 64)) % 16777216;
  temp = (temp ^ (temp / 32)) % 16777216;
  temp = (temp ^ (temp * 2048)) % 16777216;
  return temp;
}

int main(int argc, char *argv[]) {
  ifstream file("input");
  string line;
  vector<ll> input;

  while (getline(file, line)) {
    input.push_back(stoll(line));
  }

  vector<ll> temp(input);
  for (auto i : ranges::views::iota(0, 2000)) {
    std::for_each(temp.begin(), temp.end(), evolve_inplace);
  }

  ll sum1 = reduce(temp.begin(), temp.end());
  cout << "Part1: " << sum1 << endl;

  map<tuple<int, int, int, int>, int> global_map;
  for (auto inp : input) {
    set<tuple<int, int, int, int>> seen_seqs;
    vector<ll> series = {inp};
    vector<int> num;
    vector<int> diffs;
    for (auto i : ranges::views::iota(0, 2000)) {
      num.push_back(series[i] % 10);
      series.push_back(evolve(series[i]));
      if (i == 0) {
        continue;
      }
      diffs.push_back(num[i] - num[i - 1]);
    }
    for (auto i = 0; i < diffs.size() - 4; i++) {
      int a, b, c, d;
      a = diffs[i];
      b = diffs[i + 1];
      c = diffs[i + 2];
      d = diffs[i + 3];
      if (!seen_seqs.contains({a, b, c, d})) {
        seen_seqs.insert({a, b, c, d});
        if (!global_map.contains({a, b, c, d})) {
          global_map[{a, b, c, d}] = 0;
        }
        global_map[{a, b, c, d}] += num[i + 4];
      }
    }
  }

  using pair_type = decltype(global_map)::value_type;
  auto pr = std::max_element(std::begin(global_map), std::end(global_map),
                             [](const pair_type &p1, const pair_type &p2) {
                               return p1.second < p2.second;
                             });
  ll sum2 = pr->second;
  cout << "Part 2: " << sum2 << endl;

  return 0;
}
