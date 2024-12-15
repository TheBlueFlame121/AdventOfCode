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

  bool operator<(const pos &rhs) {
    if (this->x == rhs.x)
      return this->y < rhs.y;
    return this->x < rhs.x;
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

set<pos> part1(grid g, pos currPos, int currVal) {
  set<pos> out;
  if (currVal == 9) {
    out.insert(currPos);
    return out;
  }
  vector<pos> dirns = {{-1, 0}, {0, 1}, {1, 0}, {0, -1}};
  for (auto dir : dirns) {
    if (g.value(currPos + dir) == currVal + 1) {
      set<pos> tmp = part1(g, currPos + dir, currVal + 1);
      out.insert(tmp.begin(), tmp.end());
    }
  }
  return out;
}

vector<pos> part2(grid g, pos currPos, int currVal) {
  vector<pos> out;
  if (currVal == 9) {
    out.push_back(currPos);
    return out;
  }
  vector<pos> dirns = {{-1, 0}, {0, 1}, {1, 0}, {0, -1}};
  for (auto dir : dirns) {
    if (g.value(currPos + dir) == currVal + 1) {
      vector<pos> tmp = part2(g, currPos + dir, currVal + 1);
      out.insert(out.end(), tmp.begin(), tmp.end());
    }
  }
  return out;
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
  vector<pos> zeros = g.find_all('0');

  long long sum1 = 0;
  for (auto start : zeros) {
    set<pos> temp = part1(g, start, 0);
    sum1 += temp.size();
  }
  cout << "Part 1: " << sum1 << endl;

  long long sum2 = 0;
  for (auto start : zeros) {
    vector<pos> temp = part2(g, start, 0);
    sum2 += temp.size();
  }

  cout << "Part 2: " << sum2 << endl;

  return 0;
}
