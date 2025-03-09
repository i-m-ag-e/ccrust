int main(void) {
  int n = 29;
  int i;
  int is_prime = 1;

  for (i = 2; i * i <= n; i++) {
    if (n % i == 0) {
      is_prime = 0;
      break;
    }
  }
  return is_prime;
}
