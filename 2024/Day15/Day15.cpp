#include <algorithm>
#include <fstream>
#include <iostream>
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

void make_move1(grid &g, int d) {
  vector<pos> dirns = {{-1, 0}, {0, 1}, {1, 0}, {0, -1}};
  pos robot = g.find_first('@');
  pos dir = dirns[d];
  vector<pos> things_to_move = {robot};
  pos temp = robot + dir;
  while (g.value(temp) != '#' && g.value(temp) != '.') {
    things_to_move.push_back(temp);
    temp = temp + dir;
  }
  if (g.value(temp) == '#')
    return;
  for (auto thing = things_to_move.rbegin(); thing != things_to_move.rend();
       thing++) {
    g.set_value(*thing + dir, g.value(*thing));
    g.set_value(*thing, '.');
  }
}

void make_move2(grid &g, int d) {
  if (d == 1 || d == 3) {
    make_move1(g, d);
    return;
  }
  vector<pos> dirns = {{-1, 0}, {0, 1}, {1, 0}, {0, -1}};
  pos robot = g.find_first('@');
  pos dir = dirns[d];
  vector<pos> things_to_move = {robot};
  set<pos> new_targets, old_targets = {robot};
  pos temp = robot + dir;
  while (true) {
    new_targets.clear();
    for (auto i : old_targets) {
      temp = i + dir;
      switch (g.value(temp)) {
      case '#':
        return;
      case '[':
        new_targets.insert(temp);
        new_targets.insert(temp + pos(0, 1));
        break;
      case ']':
        new_targets.insert(temp);
        new_targets.insert(temp + pos(0, -1));
        break;
      case '.':
        break;
      }
    }
    if (new_targets.size() == 0)
      break;
    old_targets = new_targets;
    things_to_move.insert(things_to_move.end(), new_targets.begin(),
                          new_targets.end());
  }
  for (auto thing = things_to_move.rbegin(); thing != things_to_move.rend();
       thing++) {
    g.set_value(*thing + dir, g.value(*thing));
    g.set_value(*thing, '.');
  }
}

ll gps(grid g, char c) {
  vector<pos> boxes = g.find_all(c);
  ll out = 0;
  for (auto b : boxes) {
    out += b.x * 100 + b.y;
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
    if (line == "") {
      break;
    }
    input.push_back(line);
  }
  while (getline(file, line)) {
    moves += line;
  }

  grid g1(input);
  for (auto i : moves) {
    if (!string("^>v<").contains(i)) {
      break;
    }
    map<char, int> move = {{'^', 0}, {'>', 1}, {'v', 2}, {'<', 3}};
    make_move1(g1, move[i]);
  }

  long long sum1 = gps(g1, 'O');
  cout << "Part 1: " << sum1 << endl;

  vector<string> inp2;
  map<char, string> expansion = {
      {'#', "##"}, {'.', ".."}, {'O', "[]"}, {'@', "@."}};
  for (auto i : input) {
    string temp;
    for (auto j : i) {
      temp += expansion[j];
    }
    inp2.push_back(temp);
  }
  grid g2(inp2);

  for (auto i : moves) {
    if (!string("^>v<").contains(i)) {
      break;
    }
    map<char, int> move = {{'^', 0}, {'>', 1}, {'v', 2}, {'<', 3}};
    make_move2(g2, move[i]);
  }

  long long sum2 = gps(g2, '[');
  cout << "Part 2: " << sum2 << endl;

  return 0;
}
