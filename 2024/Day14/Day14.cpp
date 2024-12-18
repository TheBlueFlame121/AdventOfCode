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
  for (auto i : p) {
    os << i << " ";
  }
  return os << endl;
}

int main(int argc, char *argv[]) {
  string filename = "input";
  ifstream file(filename);
  string line;

  vector<pair<pos, pos>> input;
  while (getline(file, line)) {
    regex r("p=([\\+-]?\\d+),([\\+-]?\\d+) v=([\\+-]?\\d+),([\\+-]?\\d+)");
    smatch m;
    regex_search(line, m, r);
    // cout << line << endl;

    pos p(stoll(m[1].str()), stoll(m[2].str()));
    pos v(stoll(m[3].str()), stoll(m[4].str()));
    // cout << p << v << endl;

    input.push_back({p, v});
  }

  int width, height;
  if (filename == "example1") {
    width = 11;
    height = 7;
  } else {
    width = 101;
    height = 103;
  }

  vector<pos> after100;
  for (auto i : input) {
    pos final = i.first + 100 * i.second;
    final.x %= width;
    if (final.x < 0)
      final.x += width;
    final.y %= height;
    if (final.y < 0)
      final.y += height;
    // cout << final << endl;
    after100.push_back(final);
  }
  ll q1 = 0, q2 = 0, q3 = 0, q4 = 0;
  // cout << input.size() << '\n';
  for (auto p : after100) {
    if (p.x < width / 2 && p.y < height / 2) {
      q1++;
    } else if (p.x > width / 2 && p.y < height / 2) {
      q2++;
    } else if (p.x < width / 2 && p.y > height / 2) {
      q3++;
    } else if (p.x > width / 2 && p.y > height / 2) {
      q4++;
    }
  }

  long long sum1 = q1 * q2 * q3 * q4;
  cout << "Part 1: " << sum1 << endl;

  // check if all robots occupy unique positions
  set<pos> intermediate;
  int t = 0;
  while (true) {
    intermediate.clear();
    for (auto i : input) {
      pos final = i.first + t * i.second;
      final.x %= width;
      if (final.x < 0)
        final.x += width;
      final.y %= height;
      if (final.y < 0)
        final.y += height;
      // cout << final << endl;
      intermediate.insert(final);
    }
    if (intermediate.size() == input.size()) {
      break;
    }
    t++;
  }

  long long sum2 = t;
  cout << "Part 2: " << sum2 << endl;

  return 0;
}
