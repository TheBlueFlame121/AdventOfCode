#include <algorithm>
#include <cinttypes>
#include <climits>
#include <fstream>
#include <iostream>
#include <limits>
#include <map>
#include <queue>
#include <ranges>
#include <set>
#include <sstream>
#include <string>
#include <tuple>
#include <vector>

using namespace std;
auto cartesian_product = ranges::views::cartesian_product;
using ll = long long;

typedef struct pos {
  int x, y;

  pos(int i, int j) {
    this->x = i;
    this->y = j;
  }

  pos operator+=(const pos &rhs) {
    pos temp(0, 0);
    temp.x += this->x + rhs.x;
    temp.y += this->y + rhs.y;
    return temp;
  }

  pos operator+(pos rhs) { return rhs += *this; }

  pos operator-() {
    pos temp(0, 0);
    temp.x = -x;
    temp.y = -y;
    return temp;
  }

  pos operator-(pos rhs) { return *this + (-rhs); }

} pos;

bool operator==(const pos lhs, const pos rhs) {
  return lhs.x == rhs.x && lhs.y == rhs.y;
}

bool operator<(const pos lhs, const pos rhs) {
  if (lhs.x == rhs.x)
    return lhs.y < rhs.y;
  return lhs.x < rhs.x;
}

pos operator*(const int scalar, const pos rhs) {
  return {rhs.x * scalar, rhs.y * scalar};
}

ostream &operator<<(ostream &os, const pos &p) {
  return os << "(" << p.x << "," << p.y << ")";
}

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

class grid {
private:
  vector<string> data;
  int width, height;

public:
  grid(vector<string> temp) {
    data = temp;
    width = data[0].length();
    height = data.size();
  }

  int get_width() { return width; }
  int get_height() { return height; }
  vector<string> get_data() { return data; }

  char value(int i, int j) {
    if (i < 0 || j < 0 || i >= height || j >= width) {
      return ' ';
    }
    return data[i][j];
  }
  char value(pos p) {
    int i = p.x, j = p.y;
    return value(i, j);
  }

  pos find_first(char c) {
    for (int i = 0; i < data.size(); i++) {
      int f;
      if ((f = data[i].find(c)) != -1) {
        return pos{i, f};
      }
    }
    return pos{-1, -1};
  }

  vector<pos> find_all(char c) {
    vector<pos> out;
    for (int i = 0; i < data.size(); i++) {
      for (int f = 0; f < data[i].size(); f++) {
        if (data[i][f] == c) {
          out.push_back({i, f});
        }
      }
    }
    return out;
  }

  void set_value(int i, int j, char c) { data[i][j] = c; }
  void set_value(pos p, char c) { set_value(p.x, p.y, c); }
};

pair<map<pos, int>, map<pos, vector<pos>>> dijkstras(grid g, pos start) {
  vector<pos> dirns = {{-1, 0}, {0, 1}, {1, 0}, {0, -1}};
  set<pos> visited;
  map<pos, int> costs;
  map<pos, vector<pos>> prev;
  auto compare = [&costs](const pos s1, const pos s2) {
    return costs[s1] > costs[s2];
  };
  priority_queue<pos, vector<pos>, decltype(compare)> neighbours(compare);
  costs[start] = 0;
  neighbours.push(start);
  while (neighbours.size() != 0) {
    pos currState = neighbours.top();
    neighbours.pop();
    visited.insert(currState);

    for (int i = 0; i < 4; i++) {
      pos temp = currState + dirns[i];
      if (g.value(temp) != ' ') {
        if (!visited.contains(temp)) {
          costs[temp] = costs[currState] + 1;
          neighbours.push(temp);
          visited.insert(temp);
          prev[temp].push_back(currState);
        } else if (costs[temp] == costs[currState] + 1) {
          prev[temp].push_back(currState);
        }
      }
    }
  }
  return {costs, prev};
}

vector<string> find_paths(pos start, pos end, map<pos, vector<pos>> prev) {
  vector<string> out;
  if (start == end) {
    return out;
  }
  for (auto p : prev[end]) {
    vector<string> temp = find_paths(start, p, prev);
    string diff;
    vector<pos> dirns = {{-1, 0}, {0, 1}, {1, 0}, {0, -1}};
    string chars = "^>v<";
    auto it = find(dirns.begin(), dirns.end(), end - p);
    diff = chars[it - dirns.begin()];
    if (temp.size() == 0) {
      out.push_back(diff);
    } else {
      for (auto s : temp) {
        out.push_back(s + diff);
      }
    }
  }
  return out;
}

vector<pos> get_all_coords(grid g) {
  vector<pos> out;
  for (int i = 0; i < g.get_height(); i++) {
    for (int j = 0; j < g.get_width(); j++) {
      out.push_back({i, j});
    }
  }
  return out;
}

string key(pos s, pos e, int depth) {
  stringstream ss;
  ss << s << e;
  return ss.str() + to_string(depth);
}

map<string, ll> cache;
ll compute_length(pos s, pos e, int depth,
                  map<pair<pos, pos>, vector<string>> shortest_paths_a,
                  grid arrows) {
  if (depth == 1) {
    // cout << "Hit the base case" << endl;
    return shortest_paths_a[{s, e}][0].length() + 1;
  }
  string k = key(s, e, depth);
  if (cache.contains(k)) {
    return cache[k];
  }
  ll optimal = numeric_limits<ll>::max();
  for (auto path : shortest_paths_a[{s, e}]) {
    ll length = 0;
    string to_trace = "A" + path + "A";
    for (int i = 0; i < to_trace.length() - 1; i++) {
      length += compute_length(arrows.find_first(to_trace[i]),
                               arrows.find_first(to_trace[i + 1]), depth - 1,
                               shortest_paths_a, arrows);
    }
    optimal = min(optimal, length);
  }
  cache[k] = optimal;
  return optimal;
}

int main(int argc, char *argv[]) {
  ifstream file("input");
  string line;
  vector<string> input;

  while (getline(file, line)) {
    input.push_back(line);
  }

  // Define the two grids
  vector<string> n = {"789", "456", "123", " 0A"};
  vector<string> a = {" ^A", "<v>"};
  grid numpad(n), arrows(a);

  // Get shortest path between any two keys in numpad
  map<pair<pos, pos>, vector<string>> shortest_paths_n;
  for (auto s : get_all_coords(numpad)) {
    if (numpad.value(s) == ' ') {
      continue;
    }
    map<pos, int> costs;
    map<pos, vector<pos>> prev;
    tie(costs, prev) = dijkstras(numpad, s);
    for (auto e : get_all_coords(numpad)) {
      if (numpad.value(e) == ' ') {
        continue;
      }
      shortest_paths_n[{s, e}] = find_paths(s, e, prev);
    }
  }
  for (auto s : get_all_coords(numpad)) {
    if (numpad.value(s) == ' ') {
      continue;
    }
    shortest_paths_n[{s, s}] = {""};
  }

  // Get shortest path between any two keys in arrows
  map<pair<pos, pos>, vector<string>> shortest_paths_a;
  for (auto s : get_all_coords(arrows)) {
    if (arrows.value(s) == ' ') {
      continue;
    }
    map<pos, int> costs;
    map<pos, vector<pos>> prev;
    tie(costs, prev) = dijkstras(arrows, s);
    for (auto e : get_all_coords(arrows)) {
      if (arrows.value(e) == ' ') {
        continue;
      }
      shortest_paths_a[{s, e}] = find_paths(s, e, prev);
    }
  }
  for (auto s : get_all_coords(arrows)) {
    if (arrows.value(s) == ' ') {
      continue;
    }
    shortest_paths_a[{s, s}] = {""};
  }

  ll sum1 = 0;
  for (auto inp : input) {
    // inp = input[4];
    string num = inp;

    // convert number to type into arrow presses
    vector<string> arrows1;
    vector<vector<string>> temp;
    string to_type = "A" + inp;
    for (int i = 0; i < to_type.size() - 1; i++) {
      pos s = numpad.find_first(to_type[i]);
      pos e = numpad.find_first(to_type[i + 1]);
      temp.push_back(shortest_paths_n[{s, e}]);
    }
    arrows1 = {""};
    for (int i = 0; i < temp.size(); i++) {
      vector<string> outtemp;
      for (auto [a, b] : cartesian_product(arrows1, temp[i])) {
        outtemp.push_back(a + b + "A");
      }
      arrows1 = outtemp;
    }
    // cout << inp << ":" << endl;
    // cout << arrows1 << endl;

    // convert arrows1 to arrows2
    vector<string> arrows2;
    for (auto inp : arrows1) {
      temp.clear();
      to_type = "A" + inp;
      for (int i = 0; i < to_type.size() - 1; i++) {
        pos s = arrows.find_first(to_type[i]);
        pos e = arrows.find_first(to_type[i + 1]);
        temp.push_back(shortest_paths_a[{s, e}]);
      }
      vector<string> arrows_temp = {""};
      for (int i = 0; i < temp.size(); i++) {
        vector<string> outtemp;
        if (temp[i].size() == 0) {
          for (auto [a, b] :
               cartesian_product(arrows_temp, (vector<string>){""})) {
            outtemp.push_back(a + b + "A");
          }
        } else {
          for (auto [a, b] : cartesian_product(arrows_temp, temp[i])) {
            outtemp.push_back(a + b + "A");
          }
        }
        arrows_temp = outtemp;
      }
      arrows2.insert(arrows2.end(), arrows_temp.begin(), arrows_temp.end());
    }

    // convert arrows2 to arrows3
    vector<string> arrows3;
    for (auto inp : arrows2) {
      temp.clear();
      to_type = "A" + inp;
      for (int i = 0; i < to_type.size() - 1; i++) {
        pos s = arrows.find_first(to_type[i]);
        pos e = arrows.find_first(to_type[i + 1]);
        temp.push_back(shortest_paths_a[{s, e}]);
      }
      vector<string> arrows_temp = {""};
      for (int i = 0; i < temp.size(); i++) {
        vector<string> outtemp;
        if (temp[i].size() == 0) {
          for (auto [a, b] :
               cartesian_product(arrows_temp, (vector<string>){""})) {
            outtemp.push_back(a + b + "A");
          }
        } else {
          for (auto [a, b] : cartesian_product(arrows_temp, temp[i])) {
            outtemp.push_back(a + b + "A");
          }
        }
        arrows_temp = outtemp;
      }
      arrows3.insert(arrows3.end(), arrows_temp.begin(), arrows_temp.end());
    }

    auto compare_length = [](const string s1, const string s2) {
      return s1.length() < s2.length();
    };

    string min_length =
        (*min_element(arrows3.begin(), arrows3.end(), compare_length));
    sum1 += stoll(num.substr(0, 3)) * min_length.length();
  }
  cout << "Part 1: " << sum1 << endl;

  ll sum2 = 0;
  for (auto inp : input) {
    string num = inp;

    // convert number to type into arrow presses
    vector<string> arrows1;
    vector<vector<string>> temp;
    string to_type = "A" + inp;
    for (int i = 0; i < to_type.size() - 1; i++) {
      pos s = numpad.find_first(to_type[i]);
      pos e = numpad.find_first(to_type[i + 1]);
      temp.push_back(shortest_paths_n[{s, e}]);
    }
    arrows1 = {""};
    for (int i = 0; i < temp.size(); i++) {
      vector<string> outtemp;
      for (auto [a, b] : cartesian_product(arrows1, temp[i])) {
        outtemp.push_back(a + b + "A");
      }
      arrows1 = outtemp;
    }
    ll optimal = numeric_limits<ll>::max();
    for (auto path : arrows1) {
      ll length = 0;
      string to_trace = "A" + path;
      for (int i = 0; i < to_trace.size() - 1; i++) {
        length += compute_length(arrows.find_first(to_trace[i]),
                                 arrows.find_first(to_trace[i + 1]), 25,
                                 shortest_paths_a, arrows);
      }
      optimal = min(optimal, length);
    }
    sum2 += stoll(num.substr(0, 3)) * optimal;
  }
  cout << "Part 2: " << sum2 << endl;

  return 0;
}
