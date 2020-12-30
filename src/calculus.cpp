#include <Rcpp.h>
using namespace Rcpp;

// Check if string is zero
bool is_zero (std::string const &x){
  return((x=="0") | (x=="(0)") | (x=="((0))") | (x=="{0}") | (x=="0+0i") | (x=="(0+0i)") | (x==""));
}

// Check if double is zero
bool is_zero (double const &x){
  return(x==0);
}

// Returns the size of a component cycle 
int dfs(int i, std::vector<int>& visited, std::vector<int>& goesTo) { 
  
  if (visited[i] == 1) 
    return 0; 
  
  visited[i] = 1; 
  int x = dfs(goesTo[i], visited, goesTo); 
  
  return (x + 1); 
} 

// [[Rcpp::export]]
std::list< std::vector<int> > cpp_partitions(int n, int max = 0, int length = 0, bool perm = false, bool fill = false, bool equal = true) { 

  // List to store all partitions
  std::list< std::vector<int> > I;  
    
  // Fallback
  if(n<=0){
    std::vector<int> v;
    v.push_back(0);
    if(fill==true && length>1){
      for(int i=1; i<length; i++) 
        v.push_back(0);
    }
    if(length<=1 || fill)
      I.push_back(v);
    return(I);
  }
  
  // An array to store a partition
  std::vector<int> p(n);          
  // Index of last element in a partition
  int k = 0;    
  // Initialize first partition as number itself 
  p[k] = n;                         
  
  // This loop first prints current partition then generates next 
  // partition. The loop stops when the current partition has all 1s 
  while (true) 
  { 
    // current partition 
    std::vector<int> v(p.begin(), p.begin() + k+1);
    // missing values to fill with zeros
    int miss = length-(k+1);
    // storing
    if( (max==0 || v[0]<=max) && (length==0 || miss==0 || (miss>0 && fill==true)) ) {
      // fill zeros
      if(fill==true){
        if(miss > 0) for(int i=0; i<miss; i++) v.push_back(0);
      }
      // ascending order
      std::reverse(v.begin(), v.end());  
      // permute 
      do {
        I.push_back(v);
      } while (perm && std::next_permutation(v.begin(), v.end()));
      
    }
    
    // Generate next partition 
    
    // Find the rightmost non-one value in p[]. Also, update the 
    // rem_val so that we know how much value can be accommodated 
    int rem_val = 0; 
    while (k >= 0 && p[k] == 1) 
    { 
      rem_val += p[k]; 
      k--; 
    } 
    
    // if k < 0, all the values are 1 so there are no more partitions 
    if (k < 0)  {
      if(equal==true || n==0){
        return(I);   
      } 
      else {
        std::list< std::vector<int> > J;
        J = cpp_partitions(n-1, max, length, perm, fill, equal);
        J.splice(J.end(), I);
        return(J);
      }
    }
    
    // Decrease the p[k] found above and adjust the rem_val 
    p[k]--; 
    rem_val++; 
    
    // If rem_val is more, then the sorted order is violated.  Divide 
    // rem_val in different values of size p[k] and copy these values at 
    // different positions after p[k] 
    while (rem_val > p[k]) 
    { 
      p[k+1] = p[k]; 
      rem_val = rem_val - p[k]; 
      k++; 
    } 
    
    // Copy rem_val to next position and increment position 
    p[k+1] = rem_val; 
    k++; 
  } 
} 

// parity 
int cpp_parity(std::vector<int> x, std::vector<int> y) {
  
  int n = x.size();
  if( n != (int) y.size())
    Rcpp::stop("x and y must be the same length");
  
  std::vector<int> x_sort = x;
  std::vector<int> y_sort = y;
  std::sort(x_sort.begin(), x_sort.end());
  std::sort(y_sort.begin(), y_sort.end());
  for (int i = 0; i < n; i++) {
    
    if(x_sort[i]!=y_sort[i])
      return(0);
    
    if(x_sort[i]!=(i+1)){
      std::replace(x.begin(), x.end(), x_sort[i], i+1);
      std::replace(y.begin(), y.end(), y_sort[i], i+1);
    }
    
  }
  
  std::vector<int> visited(n+1);
  std::vector<int> goesTo(n+1);
  
  for (int i = 0; i < n; i++) {
    if(goesTo[x[i]] == 0)
      goesTo[x[i]] = y[i];
    else
      return(0);
  }
  
  int transpositions = 0;
  for (int i = 1; i <= n; i++) {
    if (visited[i] == 0) {
      int ans = dfs(i, visited, goesTo);
      transpositions += ans - 1;
    }
  }
  
  // odd permutation
  if((transpositions%2)==1)
    return -1;
  
  // even permutation
  return 1;
}

// [[Rcpp::export]]
std::vector<int> cpp_parity(Rcpp::NumericMatrix x, Rcpp::NumericMatrix y) { 

  int n_x  = x.nrow();
  int n_y  = y.nrow();
  std::vector<int> parity(n_x*n_y);
  
  int k = 0;
  for(int j=0; j<n_y; j++) for(int i=0; i<n_x; i++) {
    Rcpp::NumericVector xx = x.row(i);
    Rcpp::NumericVector yy = y.row(j);
    parity[k] = cpp_parity(as<std::vector<int> >(xx), as<std::vector<int> >(yy));
    k++;
  }
  
  return(parity);
} 

// [[Rcpp::export]]
std::vector<std::string> cpp_paste(std::vector<std::string> const &x, std::vector<std::string> const &y, std::string const sep) {
  
  int n_x = x.size();
  int n_y = y.size();
  if( (n_x!=n_y) && (n_x!=1) && (n_y!=1)) 
    Rcpp::stop("x and y must be the same length");
  
  int n = std::max(n_x,n_y);
  std::vector<std::string> out(n);
  
  bool one_x = (n_x==1);
  bool one_y = (n_y==1);
  
  // x * y
  if(sep==" * ") {
    if(one_x) {
      if(is_zero(x[0])){
        for(int i=0; i<n; i++){
            out[i] = "0";  
        }
      }
      else {
        for(int i=0; i<n; i++){
          if(is_zero(y[i]))
            out[i] = "0";
          else 
            out[i] = x[0] + sep + y[i];
        }
      }
    }
    else if(one_y) {
      if(is_zero(y[0])){
        for(int i=0; i<n; i++){
          out[i] = "0";  
        }
      }
      else {
        for(int i=0; i<n; i++){
          if(is_zero(x[i]))
            out[i] = "0";
          else 
            out[i] = x[i] + sep + y[0];
        }
      }
    }
    else {
      for(int i=0; i<n; i++){
        if(is_zero(x[i]) || is_zero(y[i]))
          out[i] = "0";
        else 
          out[i] = x[i] + sep + y[i];
      }
    }
  }
  
  // x / y
  if(sep==" / ") {
    if(one_x) {
      if(is_zero(x[0])){
        for(int i=0; i<n; i++){
          out[i] = "0";  
        }
      }
      else {
        for(int i=0; i<n; i++){
          if(x[0]==y[i])
            out[i] = "1";
          else
            out[i] = x[0] + sep + y[i];
        }
      }
    }
    else if(one_y) {
      for(int i=0; i<n; i++){
        if(is_zero(x[i]))
          out[i] = "0";
        else if(x[i]==y[0])
          out[i] = "1";
        else
          out[i] = x[i] + sep + y[0];
      }
    }
    else {
      for(int i=0; i<n; i++){
        if(is_zero(x[i]))
          out[i] = "0";
        else if(x[i]==y[i])
          out[i] = "1";
        else
          out[i] = x[i] + sep + y[i];
      }
    }
  }
  
  // x + y
  if(sep==" + ") {
    if(one_x) {
      if(is_zero(x[0])){
        for(int i=0; i<n; i++){
          out[i] = y[i];  
        }
      }
      else {
        for(int i=0; i<n; i++){
          if(is_zero(y[i]))
            out[i] = x[0];
          else 
            out[i] = x[0] + sep + y[i];
        }
      }
    }
    else if(one_y) {
      if(is_zero(y[0])){
        for(int i=0; i<n; i++){
          out[i] = x[i];  
        }
      }
      else {
        for(int i=0; i<n; i++){
          if(is_zero(x[i]))
            out[i] = y[0];
          else 
            out[i] = x[i] + sep + y[0];
        }
      }
    }
    else {
      for(int i=0; i<n; i++){
        bool zero_x = is_zero(x[i]);
        bool zero_y = is_zero(y[i]);
        if(zero_x){
          if(zero_y)
            out[i] = "0";
          else
            out[i] = y[i];
        }
        else if(zero_y){
          if(zero_x)
            out[i] = "0";
          else
            out[i] = x[i];
        }
        else {
          out[i] = x[i] + sep + y[i];
        }
      }
    }
  }
  
  // x - y
  if(sep==" - ") {
    if(one_x) {
      if(is_zero(x[0])){
        for(int i=0; i<n; i++){
          if(is_zero(y[i]))
            out[i] = "0";
          else
            out[i] = sep + y[i];  
        }
      }
      else {
        for(int i=0; i<n; i++){
          if(is_zero(y[i]))
            out[i] = x[0];
          else if(x[0]==y[i])
            out[i] = "0";
          else
            out[i] = x[0] + sep + y[i];
        }
      }
    }
    else if(one_y) {
      if(is_zero(y[0])){
        for(int i=0; i<n; i++){
          out[i] = x[i];  
        }
      }
      else {
        for(int i=0; i<n; i++){
          if(is_zero(x[i]))
            out[i] = sep + y[0];
          else if(x[i]==y[0])
            out[i] = "0";
          else
            out[i] = x[i] + sep + y[0];
        }
      }
    }
    else {
      for(int i=0; i<n; i++){
        bool zero_x = is_zero(x[i]);
        bool zero_y = is_zero(y[i]);
        if(zero_x){
          if(zero_y)
            out[i] = "0";
          else
            out[i] = sep + y[i];
        }
        else if(zero_y){
          if(zero_x)
            out[i] = "0";
          else
            out[i] = x[i];
        }
        else {
          if(x[i]==y[i])
            out[i] = "0";
          else
            out[i] = x[i] + sep + y[i];
        }
      }
    }
  }
  
  return(out);
}

// numeric paste
std::vector<double> cpp_paste(std::vector<double> const &x, std::vector<double> const &y, std::string const sep) {
  
  int n_x = x.size();
  int n_y = y.size();
  if((n_x!=n_y) && (n_x!=1) && (n_y!=1)) 
    Rcpp::stop("x and y must be the same length");
  
  int n = std::max(n_x,n_y);
  std::vector<double> out(n);

  bool one_x = (n_x==1);
  bool one_y = (n_y==1);
  
  // x * y
  if(sep==" * ") {
    if(one_x) {
      for(int i=0; i<n; i++){
        out[i] = x[0] * y[i];
      }
    }
    else if(one_y) {
      for(int i=0; i<n; i++){
        out[i] = x[i] * y[0];
      }
    }
    else {
      for(int i=0; i<n; i++){
        out[i] = x[i] * y[i];
      }
    }
  }
  
  // x / y
  if(sep==" / ") {
    if(one_x) {
      for(int i=0; i<n; i++){
        out[i] = x[0] / y[i];
      }
    }
    else if(one_y) {
      for(int i=0; i<n; i++){
        out[i] = x[i] / y[0];
      }
    }
    else {
      for(int i=0; i<n; i++){
        out[i] = x[i] / y[i];
      }
    }
  }
  
  // x + y
  if(sep==" + ") {
    if(one_x) {
      for(int i=0; i<n; i++){
        out[i] = x[0] + y[i];
      }
    }
    else if(one_y) {
      for(int i=0; i<n; i++){
        out[i] = x[i] + y[0];
      }
    }
    else {
      for(int i=0; i<n; i++){
        out[i] = x[i] + y[i];
      }
    }
  }
  
  // x - y
  if(sep==" - ") {
    if(one_x) {
      for(int i=0; i<n; i++){
        out[i] = x[0] - y[i];
      }
    }
    else if(one_y) {
      for(int i=0; i<n; i++){
        out[i] = x[i] - y[0];
      }
    }
    else {
      for(int i=0; i<n; i++){
        out[i] = x[i] - y[i];
      }
    }
  }
  
  return(out);
}

// [[Rcpp::export]]
std::string cpp_collapse(std::vector<std::string> const &x, std::string const sep) {
  
  int n = x.size();
  std::string s = x[0];
  
  if(n>1){
    
    if(sep==" + "){
      if(is_zero(s))
        s = "0";  
      for(int i=1; i<n; i++){
        if(!is_zero(x[i])){
          if(s=="0")
            s = x[i];
          else
            s += sep + x[i];  
        }
      }
    }
    
    if(sep==" * "){
      if(is_zero(s))
        return("0");  
      for(int i=1; i<n; i++) {
        if(is_zero(x[i]))
          return("0");
        else 
          s += sep + x[i];
      }
    }
    
  }
  
  return(s);
}

// collapse
double cpp_collapse(std::vector<double> const &x, std::string const sep) {
  
  int n = x.size();
  double s = x[0];
  
  if(n>1){
    
    if(sep==" * ") 
      for(int i=1; i<n; i++)
        s = s * x[i];
    
    if(sep==" + ")
      for(int i=1; i<n; i++)
        s = s + x[i];
    
  }
  
  return(s);
}

// [[Rcpp::export]]
std::string cpp_inner(std::vector<std::string> const &x, std::vector<std::string> const &y) { 
  
  return(cpp_collapse(cpp_paste(x, y, " * "), " + "));
  
}

// numeric inner
double cpp_inner(std::vector<double> const &x, std::vector<double> const &y) { 
  
  return(std::inner_product(x.begin(), x.end(), y.begin(), 0.0));
  
}

// [[Rcpp::export]]
std::vector<std::string> cpp_outer(std::vector<std::string> const &x, std::vector<std::string> const &y) { 
  
  int n_x = x.size();
  int n_y = y.size();
  
  std::vector<std::string> out(n_x*n_y);
  
  int k = 0;
  for(int i=0; i<n_y; i++) for(int j=0; j<n_x; j++) {
    
    if(is_zero(x[j]) || is_zero(y[i])) 
      out[k] = "0";
    else 
      out[k] = x[j] + " * " + y[i];
    
    k++;
  }
  
  return(out);
} 

// einstein Template
template <typename T>
std::vector<T> cpp_einstein(std::vector<T> const &x, std::vector<T> const &y, std::vector<int> const &dim, bool drop) { 
  
  int n_tmp = 1;
  for(unsigned int i=0; i<dim.size(); i++) 
    n_tmp = n_tmp*dim[i];
  
  int n_x = x.size()/n_tmp;
  int n_y = y.size()/n_tmp;
  
  std::vector<T> z;
  if(drop) z.resize(n_x*n_y);
  
  std::vector<T> tmp_x(n_tmp);
  std::vector<T> tmp_y(n_tmp);
  
  int n = 0;
  for(int i=0; i<n_y; i++) {
    
    for(int k=0; k<n_tmp; k++){
      tmp_y[k] = y[i+k*n_y];
    }
    
    for(int j=0; j<n_x; j++) {
      
      for(int k=0; k<n_tmp; k++){
        tmp_x[k] = x[j+k*n_x];
      }
      
      if(drop){
        
        z[n] = cpp_inner(tmp_x, tmp_y);
        n++;
        
      }
      else {
        
        tmp_x = cpp_paste(tmp_x, tmp_y, " * ");
        z.insert(z.end(), tmp_x.begin(), tmp_x.end());
        
      }
      
    }
    
  }
  
  return(z);
}

// [[Rcpp::export]]
SEXP cpp_einstein(SEXP const &x, SEXP const &y, std::vector<int> const &dim, bool drop = true){
  
  if(Rf_isNumber(x) && Rf_isNumber(y)){
    return(wrap(cpp_einstein<double>(as< std::vector<double> >(x), as< std::vector<double> >(y), dim, drop)));    
  }
  else {
    return(wrap(cpp_einstein<std::string>(as< std::vector<std::string> >(x), as< std::vector<std::string> >(y), dim, drop)));    
  }
  
}

// trace Template
template <typename T>
std::vector<T> cpp_trace(std::vector<T> const &x, std::vector<int> const &dim, bool drop) { 
  
  int d   = dim[0];
  int d_n = dim.size();
  int n_z = x.size()/(std::pow(static_cast<double>(d), d_n));
  
  std::vector<T> z;
  if(drop) z.resize(n_z);

  std::vector<T> tmp_x(d);
    
  int f = 0;
  for(int i=1; i<=d_n; i++)
    f += std::pow(static_cast<double>(d), i-1);
  
  int chunk = n_z*f;
  
  int n = 0;
  for(int i=0; i<n_z; i++) {
    
    for(int k=0; k<d; k++){
      tmp_x[k] = x[i+chunk*k];
    }
    
    if(drop){
      
      z[n] = cpp_collapse(tmp_x, " + ");
      n++;  
      
    }
    else {
      
      z.insert(z.end(), tmp_x.begin(), tmp_x.end());
      
    }
    
  }
  
  return(z);
}

// [[Rcpp::export]]
SEXP cpp_trace(SEXP const &x, std::vector<int> const &dim, bool drop = true){
  
  if(Rf_isNumber(x)){
    return(wrap(cpp_trace<double>(as< std::vector<double> >(x), dim, drop)));    
  }
  else {
    return(wrap(cpp_trace<std::string>(as< std::vector<std::string> >(x), dim, drop)));    
  }
  
}

// symbolic det 
std::string cpp_det_term(int i, std::string x, std::string det){
  
  if(is_zero(x) || is_zero(det))
    return("0");
  
  if(i % 2 != 0)
    return("-" + x + "*(" + det + ")");  
  
  return(x + "*(" + det + ")");
  
}

// numeric det
double cpp_det_term(int i, double x, double det){
  
  return(std::pow(static_cast<double>(-1), i) * x * det);
  
}

// det Template
template <typename T>
T cpp_det(std::vector<T> const &x, int n) { 
  
  if((n==1) || (x.size()==1))
    return(x[0]);
  
  std::vector<T> det(n);
    
  for(int i=0; i<n; i++) if(!is_zero(x[i])) {
    
    int m = n-1;
    std::vector<T> minor(std::pow(static_cast<double>(m), 2));
    
    int l = 0;
    for(int ii=1; ii<n; ii++){
      for(int jj=0; jj<n; jj++){
        if(jj!=i){
          minor[l] = x[jj+ii*n];
          l++;
        }
      }  
    }
    
    det[i] = cpp_det_term(i, x[i], cpp_det(minor, m));
    
  }
    
  return(cpp_collapse(det, " + "));
}

// [[Rcpp::export]]
SEXP cpp_det(SEXP const &x, int n){
  
  if(Rf_isNumber(x)){
    return(wrap(cpp_det<double>(as< std::vector<double> >(x), n)));    
  }
  else {
    return(wrap(cpp_det<std::string>(as< std::vector<std::string> >(x), n)));    
  }
  
}
