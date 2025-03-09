int main(void) {
  int x = 0;
  int y = 1;
  while (x < 50) {
    y += x;
    x += 2;
  }
  return y;
}