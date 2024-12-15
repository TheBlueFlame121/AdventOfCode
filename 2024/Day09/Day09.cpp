#include <algorithm>
#include <bitset>
#include <cmath>
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

void printdisk2(vector<pair<int, int>> disk2) {
  for (auto i : disk2) {
    cout << "{" << i.first << "," << i.second << "} ";
  }
  cout << endl;
}

int main(int argc, char *argv[]) {
  string filename = "input";
  ifstream file(filename);
  string line;

  string input;
  while (getline(file, line)) {
    input = line;
  }

  cout << input << endl;

  vector<int> disk;
  bool num = true;
  int val = 0;
  for (auto n : input) {
    if (num) {
      for (int i = 0; i < n - '0'; i++) {
        disk.push_back(val);
      }
      val++;
      num ^= true;
    } else {
      for (int i = 0; i < n - '0'; i++) {
        disk.push_back(-1);
      }
      num ^= true;
    }
  }

  // for (auto i : disk) {
  //   cout << i << " ";
  // }
  // cout << endl;

  int empty_idx = 0, num_idx = disk.size() - 1;
  while (empty_idx < num_idx) {
    while (disk[empty_idx] != -1) {
      empty_idx++;
    }
    while (disk[num_idx] == -1) {
      num_idx--;
    }
    if (empty_idx > num_idx)
      break;
    disk[empty_idx] = disk[num_idx];
    empty_idx += 1;
    disk[num_idx] = -1;
    num_idx -= 1;
  }

  // for (auto i : disk) {
  //   cout << i << " ";
  // }
  // cout << endl;

  long long checksum = 0;
  for (int i = 0; i < disk.size(); i++) {
    if (disk[i] == -1) {
      break;
    }
    checksum += i * disk[i];
  }
  cout << checksum << endl;

  long long sum1 = checksum;
  cout << "Part 1: " << sum1 << endl;

  vector<pair<int, int>> disk2;
  num = true;
  val = 0;
  for (auto n : input) {
    if (num) {
      disk2.push_back({n - '0', val});
      val++;
      num ^= true;
    } else {
      if (n - '0' != 0) {
        disk2.push_back({n - '0', -1});
      }
      num ^= true;
    }
  }
  // printdisk2(disk2);
  int file_idx = disk2.size() - 1;
  int space_idx = 0;
  while (file_idx > 0) {
    // get size of last block
    while (disk2[file_idx].second == -1) {
      file_idx--;
    }
    // find space till you hit the block
    for (space_idx = 0; space_idx < file_idx; space_idx++) {
      if (disk2[space_idx].second != -1)
        continue;
      if (disk2[space_idx].first == disk2[file_idx].first) {
        // move file here
        disk2[space_idx].second = disk2[file_idx].second;
        // make file idx blank
        disk2[file_idx].second = -1;
        // break
        break;
      }
      if (disk2[space_idx].first > disk2[file_idx].first) {
        int extra = disk2[space_idx].first - disk2[file_idx].first;
        // move and split
        disk2[space_idx].first = disk2[file_idx].first;
        disk2[space_idx].second = disk2[file_idx].second;
        disk2.insert(disk2.begin() + space_idx + 1, {extra, -1});
        // make file idx blank
        file_idx += 1;
        disk2[file_idx].second = -1;
        // break
        break;
      }
    }
    // decrement last block
    file_idx--;
    // printdisk2(disk2);
  }

  vector<int> clean_disk2;
  for (auto i : disk2) {
    for (int j = 0; j < i.first; j++) {
      clean_disk2.push_back(i.second);
    }
  }

  // for (auto i : clean_disk2) {
  //   cout << i << " ";
  // }
  // cout << endl;

  long long checksum2 = 0;
  for (int i = 0; i < clean_disk2.size(); i++) {
    if (clean_disk2[i] == -1) {
      continue;
    }
    checksum2 += i * clean_disk2[i];
  }

  long long sum2 = checksum2;
  cout << "Part 2: " << sum2 << endl;

  return 0;
}
