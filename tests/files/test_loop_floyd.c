int main(void) {
  int i;
  int j;
  int num = 1;
  for (i = 1; i <= 5; i++) {
    for (j = 1; j <= i; j++) {
      num++;
    }
  }
  return num;
}