#include <climits>
#include <cmath>
#include <fstream>
#include <iostream>
#include <string>
#include <vector>

using namespace std;
using ll = long long;

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

vector<int> output1;

int combo(int A, int B, int C, int operand) {
  switch (operand) {
  case 4:
    return A;
  case 5:
    return B;
  case 6:
    return C;
  default:
    return operand;
  }
}

int adv(int &A, int &B, int &C, int operand, int pointer) {
  int exp = combo(A, B, C, operand);
  A = A >> exp;
  return pointer + 2;
}

int bxl(int &A, int &B, int &C, int operand, int pointer) {
  B = B ^ operand;
  return pointer + 2;
}

int bst(int &A, int &B, int &C, int operand, int pointer) {
  int num = combo(A, B, C, operand);
  B = num % 8;
  return pointer + 2;
}

int jnz(int &A, int &B, int &C, int operand, int pointer) {
  if (A == 0) {
    return pointer + 2;
  }
  return operand;
}

int bxc(int &A, int &B, int &C, int operand, int pointer) {
  B = B ^ C;
  return pointer + 2;
}

int out(int &A, int &B, int &C, int operand, int pointer) {
  int num = combo(A, B, C, operand);
  output1.push_back(num % 8);
  return pointer + 2;
}

int bdv(int &A, int &B, int &C, int operand, int pointer) {
  int exp = combo(A, B, C, operand);
  B = A >> exp;
  return pointer + 2;
}

int cdv(int &A, int &B, int &C, int operand, int pointer) {
  int exp = combo(A, B, C, operand);
  C = A >> exp;
  return pointer + 2;
}

ll part2(vector<int> program, ll ans) {
  if (program.size() == 0) {
    return ans;
  }
  ll a, b, c;
  for (int i = 0; i < 8; i++) {
    a = (ans << 3) + i;
    b = a % 8;
    b = b ^ 2;
    c = a >> b;
    b = b ^ 7;
    b = b ^ c;
    if (b % 8 == program[program.size() - 1]) {
      vector<int> temp = program;
      temp.pop_back();
      ll sub = part2(temp, a);
      if (sub == -1) {
        continue;
      }
      return sub;
    }
  }
  return -1;
}

void run_program(int &A, int &B, int &C, vector<int> program) {
  int i = 0;
  while (i < program.size()) {
    switch (program[i]) {
    case 0:
      i = adv(A, B, C, program[i + 1], i);
      break;
    case 1:
      i = bxl(A, B, C, program[i + 1], i);
      break;
    case 2:
      i = bst(A, B, C, program[i + 1], i);
      break;
    case 3:
      i = jnz(A, B, C, program[i + 1], i);
      break;
    case 4:
      i = bxc(A, B, C, program[i + 1], i);
      break;
    case 5:
      i = out(A, B, C, program[i + 1], i);
      break;
    case 6:
      i = bdv(A, B, C, program[i + 1], i);
      break;
    case 7:
      i = cdv(A, B, C, program[i + 1], i);
      break;
    }
  }
}

int main(int argc, char *argv[]) {
  string filename = "input";
  ifstream file(filename);
  string line;

  vector<int> program;
  int A, B, C;
  getline(file, line);
  A = stoi(line.substr(12));
  getline(file, line);
  B = stoi(line.substr(12));
  getline(file, line);
  C = stoi(line.substr(12));
  getline(file, line);
  getline(file, line);
  for (auto i : split(line.substr(9), ",")) {
    program.push_back(stoi(i));
  }

  run_program(A, B, C, program);
  cout << "Part 1: ";
  for (int i = 0; i < output1.size() - 1; i++) {
    cout << output1[i] << ",";
  }
  cout << output1[output1.size() - 1] << endl;

  // Part 2 hard code
  ll sum2 = part2(program, 0);
  cout << "Part 2: " << sum2 << endl;

  return 0;
}
