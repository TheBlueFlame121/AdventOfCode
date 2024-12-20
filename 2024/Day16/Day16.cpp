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
      return '1';
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

typedef struct state {
  pos p;
  int dir;
} state;

bool operator<(const state lhs, const state rhs) {
  if (lhs.p == rhs.p) {
    return lhs.dir < rhs.dir;
  }
  return lhs.p < rhs.p;
}

bool operator==(const state lhs, const state rhs) {
  return lhs.p == rhs.p && lhs.dir == rhs.dir;
}

ostream &operator<<(ostream &os, const state &p) {
  return os << "{" << p.p << "," << p.dir << "}";
}

pair<map<state, int>, map<state, vector<state>>> dijkstras(grid g, pos start) {
  vector<pos> dirns = {{-1, 0}, {0, 1}, {1, 0}, {0, -1}};
  set<state> visited;
  map<state, int> costs;
  map<state, vector<state>> prev;
  auto compare = [&costs](const state s1, const state s2) {
    return costs[s1] > costs[s2];
  };
  priority_queue<state, vector<state>, decltype(compare)> neighbours(compare);
  costs[{start, 1}] = 0;
  neighbours.push({start, 1});
  while (neighbours.size() != 0) {
    state currState = neighbours.top();
    neighbours.pop();
    visited.insert(currState);

    state temp = {currState.p, (currState.dir + 1) % 4};
    if (!visited.contains(temp)) {
      costs[temp] = costs[currState] + 1000;
      neighbours.push(temp);
      visited.insert(temp);
      prev[temp].push_back(currState);
    } else if (costs[temp] == costs[currState] + 1000) {
      prev[temp].push_back(currState);
    }

    temp = {currState.p, (currState.dir + 2) % 4};
    if (!visited.contains(temp)) {
      costs[temp] = costs[currState] + 2000;
      neighbours.push(temp);
      visited.insert(temp);
      prev[temp].push_back(currState);
    } else if (costs[temp] == costs[currState] + 2000) {
      prev[temp].push_back(currState);
    }

    temp = {currState.p, (currState.dir + 3) % 4};
    if (!visited.contains(temp)) {
      costs[temp] = costs[currState] + 1000;
      neighbours.push(temp);
      visited.insert(temp);
      prev[temp].push_back(currState);
    } else if (costs[temp] == costs[currState] + 1000) {
      prev[temp].push_back(currState);
    }

    temp = {currState.p + dirns[currState.dir], currState.dir};
    if (g.value(temp.p) == '.' || g.value(temp.p) == 'E') {
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
  return {costs, prev};
}

set<state> get_states_in_paths(map<state, vector<state>> prev, state end,
                               state start) {
  vector<state> currStates = {end};
  vector<state> temp;
  set<state> out;
  out.insert(end);
  while (currStates.size() != 0) {
    temp.clear();
    for (auto i : currStates) {
      for (auto j : prev[i]) {
        if (j != start) {
          temp.push_back(j);
        }
        out.insert(j);
      }
    }
    currStates = temp;
  }

  return out;
}

set<pos> get_tiles_in_paths(set<state> states_in_paths) {
  set<pos> out;
  for (auto i : states_in_paths) {
    out.insert(i.p);
  }
  return out;
}

int main(int argc, char *argv[]) {
  string filename = "input";
  ifstream file(filename);
  string line;

  vector<string> input;
  string moves;
  while (getline(file, line)) {
    input.push_back(line);
  }
  grid g(input);
  // cout << g.get_data();

  set<pair<pos, int>> visited;
  pos S = g.find_first('S');
  pos E = g.find_first('E');

  map<state, int> costs;
  map<state, vector<state>> prev;
  tie(costs, prev) = dijkstras(g, S);
  int a = INT_MAX, b = INT_MAX;
  if (costs.contains({E, 1})) {
    a = costs[{E, 1}];
  }
  if (costs.contains({E, 0})) {
    b = costs[{E, 0}];
  }
  state end = {E, a < b ? 1 : 0};

  ll sum1 = costs[end];
  cout << "Part 1: " << sum1 << endl;

  set<state> states_in_paths = get_states_in_paths(prev, end, {S, 1});
  set<pos> tiles_in_paths = get_tiles_in_paths(states_in_paths);
  long long sum2 = tiles_in_paths.size();
  cout << "Part 2: " << sum2 << endl;

  return 0;
}
