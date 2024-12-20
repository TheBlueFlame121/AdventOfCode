#include <algorithm>
#include <fstream>
#include <iostream>
#include <limits>
#include <map>
#include <regex>
#include <set>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>

using namespace std;
using ll = long long;

typedef struct pos {
  int x, y;

  pos(int i, int j) {
    this->x = i;
    this->y = j;
  }

  bool operator==(const pos &rhs) {
    return this->x == rhs.x && this->y == rhs.y;
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

map<string, ll> cache_after, cache_till;

string key(pos p, int d) {
  stringstream ss;
  ss << p << d;
  return ss.str();
}

ll part1(grid g, pos currPos, int currDir, int cost,
         set<pair<pos, int>> visited) {
  string k = key(currPos, currDir);
  if (cache_after.contains(k)) {
    return cache_after[k] + cost;
  }
  visited.insert({currPos, currDir});
  // cout << currPos << endl;
  vector<ll> vals;
  vector<pos> dirns = {{-1, 0}, {0, 1}, {1, 0}, {0, -1}};
  ll out = numeric_limits<ll>::max();
  for (auto i : {0, 1, 3, 2}) {
    int newDir = (currDir + i) % 4;
    pos temp = currPos + dirns[newDir];
    if (g.value(temp) == 'E') {
      if (i == 0) {
        return cost + 1;
      } else if (i == 1 || i == 3) {
        return cost + 1001;
      } else {
        return cost + 2001;
      }
    }
    if (g.value(temp) == '.' && !visited.contains({temp, newDir})) {
      if (i == 0) {
        vals.push_back(part1(g, temp, newDir, cost + 1, visited));
      }
    }
    if (!visited.contains({currPos, newDir})) {
      if (i == 2) {
        vals.push_back(part1(g, currPos, newDir, cost + 2000, visited));
      } else if (i == 0) {
        vals.push_back(part1(g, currPos, newDir, cost, visited));
      } else {
        vals.push_back(part1(g, currPos, newDir, cost + 1000, visited));
      }
    }
  }
  if (vals.size() == 0) {
    return out / 2;
  }
  out = *min_element(vals.begin(), vals.end());
  cache_after[k] = out - cost;
  return out;
}
// ll part1(grid g, pos currPos, int currDir, int cost, set<pos> visited) {
//   ll out = numeric_limits<ll>::max();
//   cout << currPos << endl;
//   visited.insert(currPos);
//   string k = key(currPos, currDir);
//
//   // See if we've already reached this state before but faster
//   if (cache_till.contains(k)) {
//     if (cache_till[k] <= cost) {
//       cost = cache_till[k];
//     } else {
//       cache_till[k] = cost;
//     }
//   } else {
//     cache_till[k] = cost;
//   }
//
//   // See if we've already calced from this point
//   if (cache_after.contains(k)) {
//     return cost + cache_after[k];
//   }
//
//   // Calculate paths after this point
//   vector<ll> vals;
//   vector<pos> dirns = {{-1, 0}, {0, 1}, {1, 0}, {0, -1}};
//   for (auto i : {-1, 0, 1}) {
//     int newDir = (currDir + i + 4) % 4;
//     pos temp = currPos + dirns[newDir];
//     if (g.value(temp) == 'E') {
//       if (i == 0) {
//         return cost + 1;
//       } else {
//         return cost + 1001;
//       }
//     }
//     if (g.value(temp) == '.' && !visited.contains(temp)) {
//       if (i == 0) {
//         vals.push_back(part1(g, temp, newDir, cost + 1, visited));
//       } else {
//         vals.push_back(part1(g, temp, newDir, cost + 1001, visited));
//       }
//     }
//   }
//   // No path ahead, return high val
//   if (vals.size() == 0) {
//     cache_after[k] = out / 2;
//     return out / 2;
//   }
//
//   // Find minimum value from potential paths and update cache_after
//   out = *min_element(vals.begin(), vals.end());
//   cache_after[k] = out - cost;
//   return out;
// }

int main(int argc, char *argv[]) {
  string filename = "example2";
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
  long long sum1 = part1(g, S, 1, 0, visited);
  cout << "Part 1: " << sum1 << endl;

  long long sum2 = 0;
  cout << "Part 2: " << sum2 << endl;

  return 0;
}
