int main(void) {
  int x = 1;
  int count = 0;
  do {
    x *= 2;
    count++;
  } while (x > 0 && count < 20);
  return count;
}
