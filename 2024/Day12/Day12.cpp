#include <algorithm>
#include <fstream>
#include <iostream>
#include <map>
#include <set>
#include <string>
#include <unordered_set>
#include <vector>

using namespace std;

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

ostream &operator<<(ostream &os, const pos &p) {
  return os << "(" << p.x << "," << p.y << ")";
}

template <class T> ostream &operator<<(ostream &os, const vector<T> &p) {
  for (auto i : p) {
    os << i << " ";
  }
  return os << endl;
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

  int value(int i, int j) {
    if (i < 0 || j < 0 || i >= height || j >= width) {
      return -1;
    }
    return data[i][j] - '0';
  }
  int value(pos p) {
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
};

vector<pos> get_region(grid g, int i, int j, vector<vector<bool>> &used) {
  vector<pos> region;
  vector<pos> dirns = {{1, 0}, {0, 1}, {-1, 0}, {0, -1}};
  region.push_back({i, j});
  used[i][j] = true;
  for (auto dir : dirns) {
    pos tmp = pos(i, j) + dir;
    if (g.value(tmp) == g.value(i, j) && !used[tmp.x][tmp.y]) {
      vector<pos> temp = get_region(g, tmp.x, tmp.y, used);
      region.insert(region.end(), temp.begin(), temp.end());
    }
  }
  return region;
}

long long perimeter(vector<pos> region) {
  long long p = 0;
  vector<pos> dirns = {{1, 0}, {0, 1}, {-1, 0}, {0, -1}};
  for (auto r : region) {
    int temp = 4;
    for (auto d : dirns) {
      if (std::find(region.begin(), region.end(), r + d) != region.end()) {
        temp--;
      }
    }
    p += temp;
  }
  return p;
}

typedef struct edge {
  pos p = {0, 0};
  string type;

  edge(pos pin, string typein) {
    p = pos(pin);
    type = string(typein);
  }
} edge;

bool sortxp(pos lhs, pos rhs) {
  if (lhs.x == rhs.x)
    return lhs.y < rhs.y;
  return lhs.x < rhs.x;
}

bool sortyp(pos lhs, pos rhs) {
  if (lhs.y == rhs.y)
    return lhs.x < rhs.x;
  return lhs.y < rhs.y;
}

bool sortx(edge lhs, edge rhs) { return sortxp(lhs.p, rhs.p); }
bool sorty(edge lhs, edge rhs) { return sortyp(lhs.p, rhs.p); }

long long sides(vector<pos> region) {
  long long p = 0;
  vector<pos> dirns = {{1, 0}, {0, 1}, {-1, 0}, {0, -1}};
  vector<edge> hori_edges, vert_edges;
  for (auto r : region) {
    pos temp = r + pos(1, 0);
    if (std::find(region.begin(), region.end(), temp) == region.end()) {
      hori_edges.push_back(edge(temp, "bottom"));
    }
    temp = r + pos(-1, 0);
    if (std::find(region.begin(), region.end(), temp) == region.end()) {
      hori_edges.push_back(edge(r, "top"));
    }
    temp = r + pos(0, -1);
    if (std::find(region.begin(), region.end(), temp) == region.end()) {
      vert_edges.push_back(edge(r, "left"));
    }
    temp = r + pos(0, 1);
    if (std::find(region.begin(), region.end(), temp) == region.end()) {
      vert_edges.push_back(edge(temp, "right"));
    }
  }
  sort(hori_edges.begin(), hori_edges.end(), sortx);
  // cout << "Hori edges: " << hori_edges;
  sort(vert_edges.begin(), vert_edges.end(), sorty);
  // cout << "Vert edges: " << vert_edges;
  p++;
  for (int i = 0; i < hori_edges.size() - 1; i++) {
    edge first = hori_edges[i];
    edge second = hori_edges[i + 1];
    if (first.p.x == second.p.x && first.p.y == second.p.y - 1 &&
        first.type == second.type) {
      continue;
    } else {
      p++;
    }
  }
  p++;
  for (int i = 0; i < vert_edges.size() - 1; i++) {
    edge first = vert_edges[i];
    edge second = vert_edges[i + 1];
    if (first.p.y == second.p.y && first.p.x == second.p.x - 1 &&
        first.type == second.type) {
      continue;
    } else {
      p++;
    }
  }

  return p;
}

int main(int argc, char *argv[]) {
  string filename = "input";
  ifstream file(filename);
  string line;

  vector<string> input;
  while (getline(file, line)) {
    input.push_back(line);
  }

  grid g(input);
  vector<vector<bool>> used;
  for (auto i : input) {
    vector<bool> temp;
    for (auto j : i) {
      temp.push_back(false);
    }
    used.push_back(temp);
  }

  vector<vector<pos>> regions;
  for (int i = 0; i < g.get_height(); i++) {
    for (int j = 0; j < g.get_width(); j++) {
      if (!used[i][j]) {
        regions.push_back(get_region(g, i, j, used));
      }
    }
  }

  long long sum1 = 0;
  for (auto reg : regions) {
    sum1 += reg.size() * perimeter(reg);
  }
  cout << "Part 1: " << sum1 << endl;

  long long sum2 = 0;
  for (auto reg : regions) {
    sum2 += reg.size() * sides(reg);
  }
  cout << "Part 2: " << sum2 << endl;

  return 0;
}
