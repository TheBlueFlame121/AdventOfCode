#include <cstdio>
#include <cstdlib>
#include <fstream>
#include <iostream>
#include <map>
#include <ostream>
#include <string>
#include <vector>

using namespace std;

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
  int get_width() { return width; }
  int get_height() { return height; }
};

int count_XMAS(grid g, int i, int j) {
  vector<vector<int>> dirns = {{-1, -1}, {-1, 0}, {-1, 1}, {0, -1},
                               {0, 1},   {1, -1}, {1, 0},  {1, 1}};
  int count = 0;
  for (auto d : dirns) {
    int x = i, y = j;
    string temp = "X";
    for (int k = 0; k < 3; k++) {
      x += d[0];
      y += d[1];
      temp += g.value(x, y);
    }
    if (temp == "XMAS") {
      count++;
    }
  }
  return count;
}

bool forms_X_MAS(grid g, int i, int j) {
  vector<vector<int>> diag1 = {{-1, -1}, {1, 1}};
  vector<vector<int>> diag2 = {{-1, 1}, {1, -1}};
  if (g.value(i, j) == 'A') {
    map<char, int> m1, m2;
    for (auto d : diag1) {
      int x = i + d[0];
      int y = j + d[1];
      m1[g.value(x, y)]++;
    }
    for (auto d : diag2) {
      int x = i + d[0];
      int y = j + d[1];
      m2[g.value(x, y)]++;
    }
    if (m1['S'] == 1 && m1['M'] == 1 && m2['S'] == 1 && m2['M'] == 1) {
      return true;
    }
  }
  return false;
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

  long long sum1 = 0;
  for (int i = 0; i < g.get_height(); i++) {
    for (int j = 0; j < g.get_width(); j++) {
      if (g.value(i, j) == 'X') {
        sum1 += count_XMAS(g, i, j);
      }
    }
  }
  cout << "Part 1: " << sum1 << endl;

  long long sum2 = 0;
  for (int i = 0; i < g.get_height(); i++) {
    for (int j = 0; j < g.get_width(); j++) {
      if (forms_X_MAS(g, i, j)) {
        sum2++;
      }
    }
  }
  cout << "Part 2: " << sum2 << endl;

  return 0;
}
