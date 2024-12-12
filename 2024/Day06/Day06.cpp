#include <algorithm>
#include <fstream>
#include <iostream>
#include <map>
#include <set>
#include <string>
#include <unordered_set>
#include <vector>

using namespace std;

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
} pos;

bool operator==(const pos &lhs, const pos &rhs) {
  return lhs.x == rhs.x && lhs.y == rhs.y;
}

bool operator<(const pos &lhs, const pos &rhs) {
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
  int get_width() { return width; }
  int get_height() { return height; }
  pos find(char c) {
    for (int i = 0; i < data.size(); i++) {
      int f;
      if ((f = data[i].find(c)) != -1) {
        return pos{i, f};
      }
    }
    return pos{-1, -1};
  }
};

int part1(grid g) {
  pos currPos = g.find('^');
  vector<vector<int>> dirns = {{-1, 0}, {0, 1}, {1, 0}, {0, -1}};
  int currDir = 0;
  set<pos> visited;
  multimap<pos, int> coordToDir;
  visited.insert(currPos);
  coordToDir.insert(pair<pos, int>(currPos, currDir));

  while (g.value(currPos) != '1') {
    pos nextPos = {currPos.x + dirns[currDir][0],
                   currPos.y + dirns[currDir][1]};
    char nextChar = g.value(nextPos);
    if (nextChar == '#') {
      // turn right
      currDir += 1;
      currDir %= 4;
      // don't update set
      // don't update currPos
    } else if (nextChar == '.' || nextChar == '^') {
      // update set
      visited.insert(nextPos);
      // update update currPos
      currPos = nextPos;
    } else if (nextChar == '1') {
      // update currPos
      currPos = nextPos;
    }
  }
  return visited.size();
}

int stuck_in_loop(grid g) {
  pos currPos = g.find('^');
  vector<vector<int>> dirns = {{-1, 0}, {0, 1}, {1, 0}, {0, -1}};
  int currDir = 0;
  set<pos> visited;
  multimap<pos, int> coordToDir;
  visited.insert(currPos);
  coordToDir.insert(pair<pos, int>(currPos, currDir));
  int t = 0;

  while (t < 1e4) {
    pos nextPos = {currPos.x + dirns[currDir][0],
                   currPos.y + dirns[currDir][1]};
    char nextChar = g.value(nextPos);
    if (nextChar == '#') {
      // turn right
      currDir += 1;
      currDir %= 4;
      // don't update set
      // don't update currPos
    } else if (nextChar == '.' || nextChar == '^') {
      // update set
      visited.insert(nextPos);
      // update update currPos
      currPos = nextPos;
    } else if (nextChar == '1') {
      // update currPos
      return false;
    }
    t++;
  }
  return true;
}

int main(int argc, char *argv[]) {
  string filename = "input";
  ifstream file(filename);
  string line;

  vector<string> input;
  while (file >> line) {
    input.push_back(line);
  }

  grid g(input);

  long long sum1 = part1(g);
  cout << "Part 1: " << sum1 << endl;

  long long sum2 = 0;
  for (int i = 0; i < input.size(); i++) {
    for (int j = 0; j < input[i].length(); j++) {
      if (input[i][j] != '.') {
        continue;
      }
      input[i][j] = '#';
      sum2 += stuck_in_loop(grid(input));
      input[i][j] = '.';
    }
    cout << i << "/" << input.size() << endl;
    cout << "\x1b[1A" << "\x1b[2K";
  }
  cout << "Part 2: " << sum2 << endl;

  return 0;
}
