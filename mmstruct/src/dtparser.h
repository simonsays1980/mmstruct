/******************************************************************************
 *
 * TODO: Project Title
 *
 * Copyright (C) 2003-2009 ascolab GmbH. All Rights Reserved.
 * Web: http://www.ascolab.com
 *
 * Author: Gerhard Gappmeier <gerhard.gappmeier@ascolab.com>
 *
 * This file is provided AS IS with NO WARRANTY OF ANY KIND, INCLUDING THE
 * WARRANTY OF DESIGN, MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 ******************************************************************************/

#ifndef __DTPARSER_H__
#define __DTPARSER_H__

#include <Rcpp.h>
#include <cctype>

using namespace std;

/* Start of each months in seconds */
static const int cml[] = { 0, 0, 2678400, 5097600, 7776000, 10368000, 13046400, 15638400,
			   18316800, 20995200, 23587200, 26265600, 28857600, 31536000 };

inline
Rcpp::NumericVector dtparseV (Rcpp::CharacterVector &charv, const int &required_comp) 
{
    const unsigned int N = charv.size();
    unsigned int i;
    int comp = 0;
    Rcpp::NumericVector res(N);
    for (i = 0; i < N; ++i) {
        const char* c = (const char*) charv(i);
        if (isdigit(*c)) { 
            double ts = 0.0;
            int y = 0, m = 0, d = 0, h = 0, mi = 0;
            // Stops for at the first delimiter
            while (isdigit(*c)) {
                y = y * 10 + (*c - '0');
                ++c;
            }
            /**
             * If only two year digits are provided, it is assumed
             * that the years are from after the year 2000.
             */
            if (y < 100) {
                y += 2000;
            }
            // Base: year 1970
            y -= 1970;
            // Account for leap years (one day more: 86400 sec)
            ts += ((int)(y + 1)/4) * 86400;
            ts += y * 31536000;
            ++comp;
            while (*c && !isdigit(*c)) {
                ++c;
            }
            if (*c) {
                while (isdigit(*c)) {
                    m = m * 10 + (*c - '0');
                    ++c;
                }
                /* Add the start time of the month */
                if (m > 0 && m < 13) {
                    ts += cml[m];
                }
                /**
                 * Check if the actual year is a leap year.
                 * This is only relevant for months after February.
                 * 1972 was a leap year, so all binaries ending
                 * in '10' are leap years.
                 * As m is ensured to be an unsigned integer
                 * a bitwise operation can be done.
                 */
                if (m > 2 && (y & 3) == 2) {
                    ts += 86400; 
                }
                ++comp;
                while (*c && !isdigit(*c)) {
                    ++c;
                }
                if (isdigit(*c)) {
                    while (*c && isdigit(*c)) {
                        d = d * 10 + (*c - '0');
                        ++c;
                    }            
                    if (d > 1) {
                        ts += (d - 1) * 86400;
                    }
                    ++comp;
                    while(*c && !isdigit(*c)) {
                        ++c;
                    }
                    if (*c) {
                        while(*c && isdigit(*c)) {
                            h = h * 10 + (*c - '0');
                            ++c;
                        }
                        ts += h * 3600;
                        ++comp;
                        while (*c && !isdigit(*c)) {
                            ++c;
                        }   
                        if (*c) {
                            while (*c && isdigit(*c)) {
                                mi = mi * 10 + (*c - '0');
                                ++c;
                            }
                            ts += mi * 60;
                            ++comp;
                            while (*c && !(isdigit(*c) || *c == '.')) {
                                ++c;
                            }
                            if (*c) {
                                /* Takes the rest of the string as a number */
                                ts += atof(c); 
                            }
                            ++comp;
                        }
                    }
                }
                if (comp >= required_comp) {
                    res(i) = ts;                
                }
            }
        }
    }
    return res;
}

inline
Rcpp::NumericVector dtparseV_no_del2 (Rcpp::CharacterVector &charv, const int &required_comp) 
{
    const unsigned int N = charv.size();
    unsigned int i;
    int comp = 0;
    Rcpp::NumericVector res(N);
    for (i = 0; i < N; ++i) {
        const char* c = (const char*) charv(i);
        const char* mc = c + 2;
        const char* dc = c + 4;
        if (isdigit(*c)) { 
            double ts = 0.0;
            int y = 0, m = 0, d = 0, h = 0, mi = 0;
            // Stops for at the first delimiter
            while (isdigit(*c) && c != mc) {
                y = y * 10 + (*c - '0');
                ++c;
            }
            y += 2000;            
            // Base: year 1970
            y -= 1970;
            // Account for leap years (one day more: 86400 sec)
            ts += ((int)(y + 1)/4) * 86400;
            ts += y * 31536000;
            ++comp;
            while (*c && !isdigit(*c)) {
                ++c;
            }
            if (*c) {
                while (isdigit(*c) && c != dc) {
                    m = m * 10 + (*c - '0');
                    ++c;
                }
                /* Add the start time of the month */
                if (m > 0 && m < 13) {
                    ts += cml[m];
                }
                /**
                 * Check if the actual year is a leap year.
                 * This is only relevant for months after February.
                 * 1972 was a leap year, so all binaries ending
                 * in '10' are leap years.
                 * As m is ensured to be an unsigned integer
                 * a bitwise operation can be done.
                 */
                if (m > 2 && (y & 3) == 2) {
                    ts += 86400; 
                }
                ++comp;
                while (*c && !isdigit(*c)) {
                    ++c;
                }
                if (isdigit(*c)) {
                    while (*c && isdigit(*c)) {
                        d = d * 10 + (*c - '0');
                        ++c;
                    }            
                    if (d > 1) {
                        ts += (d - 1) * 86400;
                    }
                    ++comp;
                    while(*c && !isdigit(*c)) {
                        ++c;
                    }
                    if (*c) {
                        while(*c && isdigit(*c)) {
                            h = h * 10 + (*c - '0');
                            ++c;
                        }
                        ts += h * 3600;
                        ++comp;
                        while (*c && !isdigit(*c)) {
                            ++c;
                        }   
                        if (*c) {
                            while (*c && isdigit(*c)) {
                                mi = mi * 10 + (*c - '0');
                                ++c;
                            }
                            ts += mi * 60;
                            ++comp;
                            while (*c && !(isdigit(*c) || *c == '.')) {
                                ++c;
                            }
                            if (*c) {
                                /* Takes the rest of the string as a number */
                                ts += atof(c); 
                            }
                            ++comp;
                        }
                    }
                }
                if (comp >= required_comp) {
                    res(i) = ts;                
                }
            }
        }
    }
    return res;
}

inline
Rcpp::NumericVector dtparseV_no_del4 (Rcpp::CharacterVector &charv, const int &required_comp) 
{
    const unsigned int N = charv.size();
    unsigned int i;
    int comp = 0;
    Rcpp::NumericVector res(N);
    for (i = 0; i < N; ++i) {
        const char* c = (const char*) charv(i);
        const char* mc = c + 4;
        const char* dc = c + 6;
        if (isdigit(*c)) { 
            double ts = 0.0;
            int y = 0, m = 0, d = 0, h = 0, mi = 0;
            // Stops for at the first delimiter
            while (isdigit(*c) && c != mc) {
                y = y * 10 + (*c - '0');
                ++c;
            }
            // Base: year 1970
            y -= 1970;
            // Account for leap years (one day more: 86400 sec)
            ts += ((int)(y + 1)/4) * 86400;
            ts += y * 31536000;
            ++comp;
            while (*c && !isdigit(*c)) {
                ++c;
            }
            if (*c) {
                while (isdigit(*c) && c != dc) {
                    m = m * 10 + (*c - '0');
                    ++c;
                }
                /* Add the start time of the month */
                if (m > 0 && m < 13) {
                    ts += cml[m];
                }
                /**
                 * Check if the actual year is a leap year.
                 * This is only relevant for months after February.
                 * 1972 was a leap year, so all binaries ending
                 * in '10' are leap years.
                 * As m is ensured to be an unsigned integer
                 * a bitwise operation can be done.
                 */
                if (m > 2 && (y & 3) == 2) {
                    ts += 86400; 
                }
                ++comp;
                while (*c && !isdigit(*c)) {
                    ++c;
                }
                if (isdigit(*c)) {
                    while (*c && isdigit(*c)) {
                        d = d * 10 + (*c - '0');
                        ++c;
                    }            
                    if (d > 1) {
                        ts += (d - 1) * 86400;
                    }
                    ++comp;
                    while(*c && !isdigit(*c)) {
                        ++c;
                    }
                    if (*c) {
                        while(*c && isdigit(*c)) {
                            h = h * 10 + (*c - '0');
                            ++c;
                        }
                        ts += h * 3600;
                        ++comp;
                        while (*c && !isdigit(*c)) {
                            ++c;
                        }   
                        if (*c) {
                            while (*c && isdigit(*c)) {
                                mi = mi * 10 + (*c - '0');
                                ++c;
                            }
                            ts += mi * 60;
                            ++comp;
                            while (*c && !(isdigit(*c) || *c == '.')) {
                                ++c;
                            }
                            if (*c) {
                                /* Takes the rest of the string as a number */
                                ts += atof(c); 
                            }
                            ++comp;
                        }
                    }
                }
                if (comp >= required_comp) {
                    res(i) = ts;                
                }
            }
        }
    }
    return res;
}

inline
double dtparsechar(const char* c, const int size, 
        const int &required_comp) 
{   
    int comp = 0;
    double res = 0;
    if (isdigit(*c)) { 
        double ts = 0.0;
        int y = 0, m = 0, d = 0, h = 0, mi = 0;
        // Stops for at the first delimiter
        while (isdigit(*c)) {
            y = y * 10 + (*c - '0'); 
            ++c;
        }
        /**
         * * If only two year digits are provided, it is assumed
         * * that the years are from after the year 2000.
         * */
        if (y < 100) {
            y += 2000;
        }
        // Base: year 1970
        y -= 1970;
        // Account for leap years (one day more: 86400 sec)
        ts += ((int)(y + 1)/4) * 86400;
        ts += y * 31536000;
        ++comp;
        while (*c && !isdigit(*c)) {
            ++c;
        }
        if (*c) {
            while (isdigit(*c)) {
                m = m * 10 + (*c - '0');
                ++c;
            }
            /* Add the start time of the month */
            if (m > 0 && m < 13) {
                ts += cml[m];
            }
            /**
             * Check if the actual year is a leap year.
             * This is only relevant for months after February.
             * 1972 was a leap year, so all binaries ending
             * in '10' are leap years.
             * As m is ensured to be an unsigned integer
             * a bitwise operation can be done.
             */
            if (m > 2 && (y & 3) == 2) {
                ts += 86400; 
            }
            ++comp;
            while (*c && !isdigit(*c)) {
                ++c;
            }
            if (isdigit(*c)) {
                while (*c && isdigit(*c)) {
                    d = d * 10 + (*c - '0');
                    ++c;
                }            
                if (d > 1) {
                    ts += (d - 1) * 86400;
                }
                ++comp;
                while(*c && !isdigit(*c)) {
                    ++c;
                }
                if (*c) {
                    while(*c && isdigit(*c)) {
                        h = h * 10 + (*c - '0');
                        ++c;
                    }
                    ts += h * 3600;
                    ++comp;
                    while (*c && !isdigit(*c)) {
                        ++c;
                    }   
                    if (*c) {
                        while (*c && isdigit(*c)) {
                            mi = mi * 10 + (*c - '0');
                            ++c;
                        }
                        ts += mi * 60;
                        ++comp;
                        while (*c && !(isdigit(*c) || *c == '.')) {
                            ++c;
                        }
                        if (*c) {
                            /* Takes the rest of the string as a number */
                            ts += atof(c); 
                        }
                        ++comp;
                    }
                }
            }
            if (comp >= required_comp) {
                res = ts;                
            }
        }
    }
    return res;
}

inline
Rcpp::NumericVector tparseV (Rcpp::CharacterVector &charv) 
{
    const unsigned int N = charv.size();
    unsigned int i;
    Rcpp::NumericVector res(N);
    for (i = 0; i < N; ++i) {
        const char* c = (const char*) charv(i);
        if (isdigit(*c)) { 
            double ts = 0.0;
            int h = 0, mi = 0;            
            while(*c && isdigit(*c)) {
                h = h * 10 + (*c - '0');
                ++c;
            }
            ts += h * 3600;
            while (*c && !isdigit(*c)) {
                ++c;
            }   
            if (*c) {
                while (*c && isdigit(*c)) {
                    mi = mi * 10 + (*c - '0');
                    ++c;
                }
                ts += mi * 60;
                while (*c && !(isdigit(*c) || *c == '.')) {
                    ++c;
                }
                if (*c) {
                    /* Takes the rest of the string as a number */
                    ts += atof(c); 
                }
            }
            res(i) = ts;                        

        }                      
    }    
    return res;
}

#endif /* __DTPARSER_H__ */



