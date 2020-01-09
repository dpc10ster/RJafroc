#include <Rcpp.h>
using namespace Rcpp;

double UNINITIALIZED = R_NegInf;

double maximum(NumericVector vector, int length)
{
  double max = UNINITIALIZED;
  for (int i = 0; i < length; i ++){
    if (vector[i] > max)
      max = vector[i];
  }
  return max;
}

double comp_phi( double a, double b )
{
  double ret = 0 ;
  if( a < b ) ret = 1.0 ;
  else if( a == b ) ret = 0.5 ;
  
  return ret ;
}


// [[Rcpp::export]]
double TrapezoidalArea(NumericVector noise, int n_noise, NumericVector signal, int n_signal )
{
  double tw = 0.0 ;
  double ret ;
  
  for( int i = 0 ; i < n_noise ; i++ ) {
    for( int j = 0 ; j < n_signal ; j++ ) {
      tw += comp_phi(noise[ i ], signal[ j ]);
    }
  }
  ret = (double)( tw / (double)( n_noise *  n_signal ) ) ;
  
  return ret ;
}

// [[Rcpp::export]]
double HrAuc( NumericMatrix nl, NumericMatrix ll, NumericVector n_lesions_per_image, NumericVector max_cases, int max_nl, int max_ll )
{
  NumericVector tp(max_cases[ 1 ]) ;
  for( int k = 0 ; k < max_cases[ 1 ] ; k++ ) {
    double max, max2 ;
    max = maximum(ll(k, _), max_ll);
    max2 = maximum(nl(k + max_cases[ 0 ], _), max_nl);
    tp[ k ] = ( max > max2 ? max : max2 ) ;
  }
  
  NumericVector fp(max_cases[ 0 ]);
  for( int k = 0 ; k < max_cases[ 0 ] ; k++ ) {
    fp[ k ] = maximum(nl(k, _), max_nl) ;
  }
  
  double ret = TrapezoidalArea( fp, max_cases[ 0 ], tp, max_cases[ 1 ] ) ;
  
  return ret ;
}

// [[Rcpp::export]]
double ROI( int ncases_nor, int ncases_abn, int max_nl, NumericVector n_les, NumericMatrix nl, NumericMatrix ll )
{
  double tw = 0.0 ;
  int nn = 0, ns = 0 ;
  double ret ;
  
  for( int k1 = 0 ; k1 < ncases_nor ; k1++ ) {
    for( int l1 = 0 ; l1 < max_nl ; l1++ ) {
      double a = nl(k1, l1) ;
      if( a == UNINITIALIZED ) 
        continue ;
      nn++ ;
      for( int k2 = 0 ; k2 < ncases_abn ; k2++ ) {
        for( int l2 = 0 ; l2 < n_les[ k2 ] ; l2++ ) {
          double b ;
          b = ll(k2, l2) ;
          tw += comp_phi( a, b ) ;
        }
      }
    }
  }
  
  for( int k1 = 0 ; k1 < ncases_abn ; k1++ ) {
    for( int l1 = 0 ; l1 < max_nl ; l1++ ) {
      double a = nl(k1 + ncases_nor, l1) ;
      if( a == UNINITIALIZED ) 
        continue ;
      nn++ ;
      for( int k2 = 0 ; k2 < ncases_abn ; k2++ ) {
        for( int l2 = 0 ; l2 < n_les[ k2 ] ; l2++ ) {
          double b ;
          b = ll(k2, l2) ;
          tw += comp_phi( a, b ) ;
        }
      }
    }
  }
  
  for( int k2 = 0 ; k2 < ncases_abn ; k2++ ) {
    ns += n_les[ k2 ] ;
  }
  
  ret = (double)( tw / (double)nn / (double)ns ) ;
  return ret ;
}

// [[Rcpp::export]]
double HrSe( NumericMatrix nl, NumericMatrix ll, NumericVector n_lesions_per_image, NumericVector max_cases, int max_nl, int max_ll )
{
  int tp_count = 0;
  for( int k = 0 ; k < max_cases[ 1 ] ; k++ ) {
    double max = UNINITIALIZED ;
    for( int l = 0 ; l < n_lesions_per_image[k] ; l++ ) {
      if( ll(k, l) > max )
        max = ll(k, l) ;
    }
    for( int l = 0 ; l < max_nl ; l++ ) {
      if( nl(k + max_cases[ 0 ], l) > max )
        max = nl(k + max_cases[ 0 ], l) ;
    }
    if (max > UNINITIALIZED) tp_count++;
  }
  
  double ret = (tp_count+0.0)/max_cases[ 1 ]; 
  return  ret;
}

// [[Rcpp::export]]
double HrSp( NumericMatrix nl, NumericMatrix ll, NumericVector n_lesions_per_image, NumericVector max_cases, int max_nl, int max_ll )
{
  int fp_count = 0;
  for( int k = 0 ; k < max_cases[ 0 ] ; k++ ) {
    double max = UNINITIALIZED ;
    for( int l = 0 ; l < max_nl ; l++ ) {
      if( nl(k, l) > max )
        max = nl(k, l) ;
    }
    if (max > UNINITIALIZED) fp_count++;
  }
  
  double ret = (fp_count+0.0)/max_cases[ 0 ]; 
  
  return 1 - ret ;
}

// [[Rcpp::export]]
double SongA1( int ncases_nor, int ncases_abn, int max_nl, int max_ll, NumericVector n_les, NumericMatrix nl, NumericMatrix ll )
{
  NumericVector x (ncases_nor) ;
  NumericVector y (ncases_abn) ;
  int p_x_0 = 0 ;
  int p_y_0 = 0 ;
  
  double tw = 0.0 ;
  double ret ;
  
  for( int k = 0 ; k < ncases_nor ; k++ ) {
    double m = 0.0f ;
    int inc = 0 ;
    for( int l = 0 ; l < max_nl ; l++ ) {
      if( nl(k, l) != UNINITIALIZED ) {
        m += nl(k, l) ;
        inc++ ;
      }
    }
    if( inc == 0 ) {
      x[ k ] = UNINITIALIZED ;
      p_x_0++ ;
    } else {
      x[ k ] = m / (double)inc ;
    }
  }
  for( int k = 0 ; k < ncases_abn ; k++ ) {
    double m1 = 0.0f ;
    int inc = 0 ;
    for( int l = 0 ; l < max_nl ; l++ ) {
      if( nl(k + ncases_nor, l) != UNINITIALIZED ) {
        m1 += nl(k + ncases_nor, l) ;
        inc++ ;
      }
    }
    if( inc == 0 ) {
      m1 = UNINITIALIZED ;
    } else {
      m1 = m1 / (double) inc ;
    }
    double m2 = 0.0f ;
    inc = 0 ;
    for( int l = 0 ; l < n_les[ k ] ; l++ ) {
      if( ll(k, l) != UNINITIALIZED ) {
        m2 += ll(k, l) ;
        inc++ ;
      }
    }
    if( inc == 0 ) {
      m2 = UNINITIALIZED ;
    } else {
      m2 = m2 / (double) inc ;
    }
    
    double m ;
    if( m1 > m2 ) m = m1 ; else m = m2 ;
    y[ k ] = m ;
    if( m == UNINITIALIZED ) {
      p_y_0++ ;
    }
  }
  
  for( int k1 = 0 ; k1 < ncases_nor ; k1++ ) {
    for( int k2 = 0 ; k2 < ncases_abn ; k2++ ) {
      if( x[ k1 ] != UNINITIALIZED && y[ k2 ] != UNINITIALIZED ) {
        if( x[ k1 ] < y[ k2 ] ) tw += 1.0 ;
        else if( x[ k1 ] == y[ k2 ] ) tw += 0.5 ;
      }
    }
  }
  ret = (double)( tw / (double)ncases_nor / (double)ncases_abn ) + (double)( ( (double)p_x_0 / (double)ncases_nor ) *  ( 1.0 - 0.5 *  (double)p_y_0 / (double)ncases_abn ) ) ;
  
  return ret ;
}

// [[Rcpp::export]]
double SongA2( int ncases_nor, int ncases_abn, int max_nl, int max_ll, NumericVector n_les, NumericMatrix nl, NumericMatrix ll )
{
  int p_x_0 = 0 ;
  int p_y_0 = 0 ;
  
  double tw = 0.0 ;
  double ret = 0.0 ;
  
  for( int k = 0 ; k < ncases_nor ; k++ ) {
    double m = maximum( nl(k, _), max_nl ) ;
    if( m == UNINITIALIZED ) {
      p_x_0++ ;
    }
  }
  for( int k = 0 ; k < ncases_abn ; k++ ) {
    double m1 = maximum( nl(k + ncases_nor, _), max_nl ) ;
    double m2 = maximum( ll(k, _), n_les[ k ] ) ;
    double m ;
    if( m1 > m2 ) m = m1 ; else m = m2 ;
    if( m == UNINITIALIZED ) {
      p_y_0++ ;
    }
  }
  
  for( int s0 = 0 ; s0 < ncases_nor ; s0++ ) {
    for( int s1 = 0 ; s1 < ncases_abn ; s1++ ) {
      int n0 = 0, n1 = 0, a = 0 ;
      for( int l = 0 ; l < max_nl ; l++ ) {
        if( nl(s0, l) != UNINITIALIZED ) n0++ ;
      }
      for( int l = 0 ; l < max_nl ; l++ ) {
        if( nl(s1 + ncases_nor, l) != UNINITIALIZED ) n1++ ;
      }
      for( int l = 0 ; l < n_les[ s1 ] ; l++ ) {
        if( ll(s1, l) != UNINITIALIZED ) a++ ;
      }
      if( n0 > 0 && ( n1 + a ) > 0  ) {
        double v1 = 0.5f ;
        double v2 = 0.0f ;
        for( int l0 = 0 ; l0 < max_nl ; l0++ ) {
          if( nl(s0, l0) != UNINITIALIZED ) {
            for( int l1 = 0 ; l1 < max_nl ; l1++ ) {
              if( nl(s1 + ncases_nor, l1) != UNINITIALIZED ) {
                v2 += comp_phi( nl(s0, l0), nl(s1 + ncases_nor, l1)) ;
              }
            }
            for( int l2 = 0 ; l2 < n_les[ s1 ] ; l2++ ) {
              if( ll(s1, l2) != UNINITIALIZED ) {
                v2 += comp_phi( nl(s0, l0), ll(s1, l2) ) ;
              }
            }
          }
        }
        v2 = v2 / (double)n0 / (double)( n1 + a ) ;
        tw += comp_phi( v1, v2 ) ;
      }
    }
  }
  
  ret = (double)( tw / (double)ncases_nor / (double)ncases_abn ) + (double)( ( (double)p_x_0 / (double)ncases_nor ) *  ( 1.0 - 0.5 *  (double)p_y_0 / (double)ncases_abn ) ) ;
  
  return ret ;
}

// [[Rcpp::export]]
double FOM_AFROC1( NumericMatrix nl, NumericMatrix ll, NumericVector n_lesions_per_image, NumericVector max_cases, int max_nl, int max_ll )
{
  int nles_total = 0 ;
  for( int k = 0 ; k < max_cases[ 1 ] ; k++ ) {
    nles_total += n_lesions_per_image[ k ] ;
  }
  
  NumericVector les(nles_total) ;
  int inc = 0 ;
  for( int k = 0 ; k < max_cases[ 1 ] ; k++ ) {
    for( int l = 0 ; l < n_lesions_per_image[ k ] ; l++ ) {
      les[ inc++ ] = ll(k, l) ;
    }
  }
  
  NumericVector fp(max_cases[ 0 ] + max_cases[ 1 ]) ;
  for( int k = 0 ; k < max_cases[ 0 ] + max_cases[ 1 ] ; k++ ) {
    fp[ k ] = maximum(nl(k, _), max_nl) ;
  }
  
  double ret = TrapezoidalArea( fp, max_cases[ 0 ] + max_cases[ 1 ], les, nles_total ) ;
  
  return ret ;
}

// [[Rcpp::export]]
double FOM_AFROC( NumericMatrix nl, NumericMatrix ll, NumericVector n_lesions_per_image, NumericVector max_cases, int max_nl, int max_ll )
{
  int nles_total = 0 ;
  for( int k = 0 ; k < max_cases[ 1 ] ; k++ ) {
    nles_total += n_lesions_per_image[ k ] ;
  }
  
  NumericVector les(nles_total) ;
  int inc = 0 ;
  for( int k = 0 ; k < max_cases[ 1 ] ; k++ ) {
    for( int l = 0 ; l < n_lesions_per_image[ k ] ; l++ ) {
      les[ inc++ ] = ll(k, l) ;
    }
  }
  
  NumericVector fp(max_cases[ 0 ]) ;
  for( int k = 0 ; k < max_cases[ 0 ] ; k++ ) {
    fp[ k ] = maximum(nl(k, _), max_nl) ;
  }
  
  double ret = TrapezoidalArea( fp, max_cases[ 0 ], les, nles_total ) ;
  
  return ret ;
}

// [[Rcpp::export]]
double FOM_wAFROC1( NumericMatrix nl, NumericMatrix ll, NumericVector n_lesions_per_image, NumericVector max_cases, int max_nl, int max_ll, NumericMatrix weights )
{
  double ret = 0.0 ;
  
  for( int na = 0 ; na < max_cases[ 1 ] ; na++ ) {
    for( int nn = 0 ; nn < max_cases[ 0 ] + max_cases[ 1 ] ; nn++ ) {
      for( int nles = 0 ; nles < n_lesions_per_image[ na ] ; nles++ ) {
        double fp = UNINITIALIZED ;
          for( int nor_index = 0 ; nor_index < max_nl ; nor_index++ )
            if( nl(nn, nor_index) > fp ) fp = nl(nn, nor_index) ;
          ret += weights(na, nles) *  comp_phi( fp, ll(na, nles) ) ;
      }
    }
  }
  ret /= (double)( max_cases[ 0 ] + max_cases[ 1 ] ) *  (double)max_cases[ 1 ] ;
  
  return (double)ret ;
}

// [[Rcpp::export]]
double FOM_wAFROC( NumericMatrix nl, NumericMatrix ll, NumericVector n_lesions_per_image, NumericVector max_cases, int max_nl, int max_ll, NumericMatrix weights )
{
  double  ret = 0.0 ;

  for( int na = 0 ; na < max_cases[ 1 ] ; na++ ) {
    for( int nn = 0 ; nn < max_cases[ 0 ] ; nn++ ) {
      for( int nles = 0 ; nles < n_lesions_per_image[ na ] ; nles++ ) {
        double fp = UNINITIALIZED ;
          for( int nor_index = 0 ; nor_index < max_nl ; nor_index++ )
            if( nl(nn, nor_index) > fp ) fp = nl(nn, nor_index) ; // this captures the highest value on normal case nn
          ret += weights(na, nles) *  comp_phi( fp, ll(na, nles) ) ;
      }
    }
  }
  ret /= (double)max_cases[ 0 ] *  (double)max_cases[ 1 ] ;

  return (double)ret ;
}

// [[Rcpp::export]]
double MaxLLF( NumericMatrix nl, NumericMatrix ll, NumericVector n_lesions_per_image, NumericVector max_cases, int max_nl, int max_ll )
{
  int TotalNumOfMarks = 0;
  for( int k = 0 ; k < max_cases[ 1 ] ; k++ ) {
    for( int l = 0 ; l < n_lesions_per_image [ k ] ; l++ ) {
      if( ll(k, l) != UNINITIALIZED )
        TotalNumOfMarks++;
    }
  }
  
  int nles_total = 0 ;
  for( int k = 0 ; k < max_cases[ 1 ] ; k++ ) {
    nles_total += n_lesions_per_image[ k ] ;
  }
  
  double ret = (TotalNumOfMarks+0.0)/nles_total ;
  
  return ret ;
}

// [[Rcpp::export]]
double MaxNLF( NumericMatrix nl, NumericMatrix ll, NumericVector n_lesions_per_image, NumericVector max_cases, int max_nl, int max_ll )
{
  int TotalNumOfMarks = 0;
  for( int k = 0 ; k < max_cases[ 0 ] ; k++ ) {
    for( int l = 0 ; l < max_nl ; l++ ) {
      if( nl(k, l) != UNINITIALIZED )
        TotalNumOfMarks++;
    }
  }
  
  double ret = (TotalNumOfMarks+0.0)/max_cases[ 0 ] ;
  
  return ret ;
}

// [[Rcpp::export]]
double MaxNLFAllCases( NumericMatrix nl, NumericMatrix ll, NumericVector n_lesions_per_image, NumericVector max_cases, int max_nl, int max_ll )
{
  int TotalNumOfMarks = 0;
  for( int k = 0 ; k < max_cases[ 0 ] + max_cases[ 1 ] ; k++ ) {
    for( int l = 0 ; l < max_nl ; l++ ) {
      if( nl(k, l) != UNINITIALIZED )
        TotalNumOfMarks++;
    }
  }
  
  double ret = (TotalNumOfMarks+0.0)/ (max_cases[ 0 ] + max_cases[ 1 ]) ;
  
  return ret ;
}

// [[Rcpp::export]]
double ExpTrnsfmSp( NumericMatrix nl, NumericMatrix ll, NumericVector n_lesions_per_image, NumericVector max_cases, int max_nl, int max_ll )
{
  int TotalNumOfMarks = 0;
  for( int k = 0 ; k < max_cases[ 0 ] ; k++ ) {
    for( int l = 0 ; l < max_nl ; l++ ) {
      if( nl(k, l) != UNINITIALIZED )
        TotalNumOfMarks++;
    }
  }
  
  double ret = (TotalNumOfMarks+0.0)/max_cases[ 0 ] ;
  
  ret = exp(-ret);
  
  return ret ;
}
