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

map<pos, int> dijkstras(grid g, pos start) {
  vector<pos> dirns = {{-1, 0}, {0, 1}, {1, 0}, {0, -1}};
  set<pos> visited;
  map<pos, int> costs;
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
      if (g.value(temp) == '.' || g.value(temp) == 'E') {
        if (!visited.contains(temp)) {
          costs[temp] = costs[currState] + 1;
          neighbours.push(temp);
          visited.insert(temp);
        }
      }
    }
  }

  return costs;
}

set<pos> points_at_dist(pos s, int dist) {
  set<pos> out;
  for (int i = 0; i <= dist; i++) {
    out.insert(s + (dist - i) * pos(0, 1) + i * pos(1, 0));
    out.insert(s + (dist - i) * pos(0, -1) + i * pos(1, 0));
  }
  return out;
}

int main(int argc, char *argv[]) {
  ifstream file("input");
  int threshold = 100;
  string line;
  vector<string> input;

  while (getline(file, line)) {
    input.push_back(line);
  }

  grid g(input);
  pos S = g.find_first('S');
  pos E = g.find_first('E');

  map<pos, int> costs = dijkstras(g, S);
  g.set_value(S, '.');
  g.set_value(E, '.');

  ll sum1 = 0;
  for (int i = 0; i < input.size(); i++) {
    for (int j = 0; j < input[i].size(); j++) {
      pos temp(i, j);
      if (g.value(temp) == '.') {
        if (g.value(temp + pos(0, 1)) == '#' &&
            g.value(temp + pos(0, 2)) == '.') {
          // horizontal cheat moment;
          int savings = abs(costs[temp] - costs[temp + pos(0, 2)]) - 2;
          if (savings >= threshold) {
            sum1++;
          }
        }
        if (g.value(temp + pos(1, 0)) == '#' &&
            g.value(temp + pos(2, 0)) == '.') {
          // vertical cheat moment;
          int savings = abs(costs[temp] - costs[temp + pos(2, 0)]) - 2;
          if (savings >= threshold) {
            sum1++;
          }
        }
      }
    }
  }
  cout << "Part 1: " << sum1 << endl;

  ll sum2 = 0;
  set<pair<pos, pos>> cheats;
  for (int i = 0; i < input.size(); i++) {
    for (int j = 0; j < input[i].size(); j++) {
      pos temp(i, j);
      if (g.value(temp) == '.') {
        for (int i = 2; i <= 20; i++) {
          for (auto final : points_at_dist(temp, i)) {
            if (!cheats.contains({temp, final}) &&
                !cheats.contains({final, temp}) && g.value(final) == '.') {
              // cheat moment;
              int savings = abs(costs[temp] - costs[final]) - i;
              cheats.insert({temp, final});
              if (savings >= threshold) {
                sum2++;
              }
            }
          }
        }
      }
    }
  }
  cout << "Part 2: " << sum2 << endl;

  return 0;
}
