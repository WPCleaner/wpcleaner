/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2020  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.data.analysis;

/**
 * Bean for holding information about analysis performance.
 */
public class AnalysisPerformance {
  long level1;
  long level2;
  long level3;
  long level4;
  long level5;
  long level5_ISBN;
  long level5_ISSN;
  long level5_PMID;
  long level5_RFC;
  long level6;

  public AnalysisPerformance() {
    level1 = 0;
    level2 = 0;
    level3 = 0;
    level4 = 0;
    level5 = 0;
    level5_ISBN = 0;
    level5_ISSN = 0;
    level5_PMID = 0;
    level5_RFC = 0;
    level6 = 0;
  }

  /**
   * @return Textual description of the object.
   * @see java.lang.Object#toString()
   */
  @Override
  public String toString() {
    return results(1000000000, "s");
  }

  /**
   * @return Results in seconds.
   */
  public String toSeconds() {
    return results(1000000000, "s");
  }

  /**
   * @return Results in milliseconds
   */
  public String toMilliSeconds() {
    return results(1000000, "ms");
  }

  /**
   * @param divider Time divider.
   * @param timeUnit Time unit.
   * @return Results.
   */
  private String results(long divider, String timeUnit) {
    long time1 = level1 / divider;
    long time2 = level2 / divider;
    long time3 = level3 / divider;
    long time4 = level4 / divider;
    long time5 = level5 / divider;
    long time5_ISBN = level5_ISBN / divider;
    long time5_ISSN = level5_ISSN / divider;
    long time5_PMID = level5_PMID / divider;
    long time5_RFC = level5_RFC / divider;
    long time6 = level6 / divider;
    StringBuilder result = new StringBuilder();
    result.append(time1 + time2 + time3 + time4 + time5 + time6);
    result.append(" ");
    result.append(timeUnit);
    result.append(" (");
    result.append(time1);
    result.append(" + ");
    result.append(time2);
    result.append(" + ");
    result.append(time3);
    result.append(" + ");
    result.append(time4);
    result.append(" + ");
    result.append(time5);
    if (time5_ISBN + time5_ISSN + time5_PMID + time5_RFC > 0) {
      result.append("=");
      result.append(time5_ISBN);
      result.append("+");
      result.append(time5_ISSN);
      result.append("+");
      result.append(time5_PMID);
      result.append("+");
      result.append(time5_RFC);
    }
    result.append(" + ");
    result.append(time6);
    result.append(")");
    return result.toString();
  }

}