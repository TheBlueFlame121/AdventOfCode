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
  char value(int i, int j) {
    if (i < 0 || j < 0 || i >= height || j >= width) {
      return '$';
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

int part1(grid g, map<char, vector<pos>> antennas) {
  set<pos> antinodes;
  for (auto freq : antennas) {
    if (freq.second.size() < 2) {
      continue;
    }
    for (int i = 0; i < freq.second.size() - 1; i++) {
      pos ant1 = freq.second[i];
      for (int j = i + 1; j < freq.second.size(); j++) {
        pos ant2 = freq.second[j];
        pos an1 = ant1 + ant1 - ant2;
        pos an2 = ant2 + ant2 - ant1;
        if (g.value(an1) != '$') {
          antinodes.insert(an1);
        }
        if (g.value(an2) != '$') {
          antinodes.insert(an2);
        }
      }
    }
  }
  return antinodes.size();
}

int part2(grid g, map<char, vector<pos>> antennas) {
  set<pos> antinodes2;
  for (auto freq : antennas) {
    if (freq.second.size() < 2) {
      continue;
    }
    for (int i = 0; i < freq.second.size() - 1; i++) {
      pos ant1 = freq.second[i];
      antinodes2.insert(ant1);

      for (int j = i + 1; j < freq.second.size(); j++) {
        pos ant2 = freq.second[j];
        antinodes2.insert(ant2);

        pos diff = ant1 - ant2;
        pos an1 = ant1 + diff;
        pos an2 = ant2 - diff;
        while (g.value(an1) != '$') {
          antinodes2.insert(an1);
          an1 = an1 + diff;
        }
        while (g.value(an2) != '$') {
          antinodes2.insert(an2);
          an2 = an2 - diff;
        }
      }
    }
  }
  return antinodes2.size();
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

  map<char, vector<pos>> antennas;

  for (int i = 0; i < input.size(); i++) {
    for (int j = 0; j < input[i].size(); j++) {
      if (input[i][j] != '.') {
        antennas[input[i][j]].push_back(pos(i, j));
      }
    }
  }

  long long sum1 = part1(g, antennas);
  cout << "Part 1: " << sum1 << endl;

  long long sum2 = part2(g, antennas);
  cout << "Part 2: " << sum2 << endl;

  return 0;
}
