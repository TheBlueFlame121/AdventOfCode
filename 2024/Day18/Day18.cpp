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
      if (g.value(temp) == '.') {
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

int part1(string filename, int gsize, int bytes) {
  ifstream file(filename);
  string line;

  vector<string> input;
  for (int i = 0; i < gsize; i++) {
    input.push_back(string(gsize, '.'));
  }
  grid g(input);

  int i = 0;
  while (getline(file, line)) {
    if (i == bytes) {
      break;
    }
    vector<string> temp = split(line, ",");
    g.set_value(stoi(temp[1]), stoi(temp[0]), '#');
    i++;
  }

  pos S = {0, 0};
  pos E = {gsize - 1, gsize - 1};

  map<pos, int> costs;
  costs = dijkstras(g, S);
  return costs[E];
}

int lines_in_a_file(string filename) {
  ifstream file(filename);
  string line;
  int out = 0;
  while (getline(file, line)) {
    out++;
  }
  return out;
}

string get_specific_line(string filename, int line_no) {
  ifstream file(filename);
  string line;
  int l = 1;
  while (getline(file, line)) {
    if (l == line_no) {
      return line;
    }
    l++;
  }
  return "";
}

string part2(string filename, int gsize, int low, int high) {
  while (low < high) {
    int mid = low + (high - low) / 2;
    if (part1(filename, gsize, mid) == 0) {
      high = mid - 1;
    } else {
      low = mid + 1;
    }
  }
  if (part1(filename, gsize, low) != 0) {
    low++;
  }
  return get_specific_line(filename, low);
}

int main(int argc, char *argv[]) {

  ll sum1 = part1("input", 71, 1024);
  cout << "Part 1: " << sum1 << endl;

  string sum2 = part2("input", 71, 12, lines_in_a_file("input"));
  cout << "Part 2: " << sum2 << endl;

  return 0;
}
