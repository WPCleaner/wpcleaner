/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.data;

import java.util.Collection;
import java.util.List;

import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithm;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithms;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.constants.WPCConfiguration;
import org.wikipediacleaner.api.constants.WPCConfigurationBoolean;
import org.wikipediacleaner.api.constants.WPCConfigurationString;


/**
 * An utility class for automatic formatting of articles.
 */
public class AutomaticFormatter {

  /**
   * Tidy up an article.
   * 
   * @param page Page.
   * @param contents Current contents.
   * @param algorithms List of Check Wiki algorithms.
   * @param usedAlgorithms Algorithms used to tidy up the article.
   * @return New contents.
   */
  public static String tidyArticle(
      Page page, String contents,
      Collection<CheckErrorAlgorithm> algorithms,
      List<CheckErrorAlgorithm> usedAlgorithms) {
    if ((page == null) || (contents == null)) {
      return contents;
    }
    EnumWikipedia wiki = page.getWikipedia();
    WPCConfiguration config = wiki.getConfiguration();

    // Fix Check Wiki errors
    if (algorithms != null) {
      for (CheckErrorAlgorithm algorithm : algorithms) {
        if (algorithm.isAvailable() &&
            CheckErrorAlgorithms.isAlgorithmActive(wiki, algorithm.getErrorNumber())) {
          String currentContents = contents;
          PageAnalysis analysis = page.getAnalysis(currentContents, true);
          contents = algorithm.automaticFix(analysis);
          if ((usedAlgorithms != null) && (!contents.equals(currentContents))) {
            usedAlgorithms.add(algorithm);
          }
        }
      }
    }

    // Auto formatting options
    if (!config.getBoolean(WPCConfigurationBoolean.AUTO_ACTIVE)) {
      return contents;
    }
    contents = fixBeforeCategory(page, contents);
    contents = fixDefaultsortCategory(page, contents);

    return contents;
  }

  /**
   * Auto formatting options: number of carriage returns before categories.
   * 
   * @param page Page.
   * @param contents Current contents.
   * @return New contents.
   */
  public static String fixBeforeCategory(Page page, String contents) {

    // Check configuration
    WPCConfiguration config = page.getWikipedia().getConfiguration();
    String option = config.getString(WPCConfigurationString.AUTO_CR_BEFORE_CATEGORY);
    if (!isValidCrOption(option)) {
      return contents;
    }
    int min = getMinCrOption(option);
    int max = getMaxCrOption(option);
    if ((min <= 0) && (max == Integer.MAX_VALUE)) {
      return contents;
    }
    PageAnalysis analysis = page.getAnalysis(contents, true);

    // Retrieve last title
    int lastTitle = 0;
    List<PageElementTitle> titles = analysis.getTitles();
    if ((titles != null) && (titles.size() > 0)) {
      lastTitle = titles.get(titles.size() - 1).getEndIndex();
    }

    // Analyze default sort
    List<PageElementFunction> defaultSorts = analysis.getDefaultSorts();
    if ((defaultSorts != null) && (defaultSorts.size() > 0)) {
      int beginSort = defaultSorts.get(0).getBeginIndex();
      if (beginSort < lastTitle) {
        return contents;
      }
      int nbCr = 0;
      int index = beginSort;
      boolean finished = false;
      while (!finished && (index > 0)) {
        index--;
        char currentChar = contents.charAt(index);
        if (currentChar == '\n') {
          nbCr++;
        } else if (currentChar != ' ') {
          finished = true;
        }
      }
      if ((nbCr == 0) || ((nbCr >= min) && (nbCr <= max))) {
        return contents;
      }
      contents = changeCarriageReturn(
          contents, index + 1,
          normalizeValue(nbCr, min, max), beginSort);
      return contents;
    }

    // Analyze first category
    List<PageElementCategory> categories = analysis.getCategories();
    if ((categories == null) || (categories.size() == 0)) {
      return contents;
    }
    int beginCat = categories.get(0).getBeginIndex();
    if (beginCat < lastTitle) {
      return contents;
    }
    int nbCr = 0;
    int index = beginCat;
    boolean finished = false;
    while (!finished && (index > 0)) {
      index--;
      char currentChar = contents.charAt(index);
      if (currentChar == '\n') {
        nbCr++;
      } else if (currentChar != ' ') {
        finished = true;
      }
    }
    if ((nbCr == 0) || ((nbCr >= min) && (nbCr <= max))) {
      return contents;
    }
    contents = changeCarriageReturn(
        contents, index + 1,
        normalizeValue(nbCr, min, max), beginCat);

    return contents;
  }

  /**
   * Auto formatting options: number of carriage returns between default sort and categories.
   * 
   * @param page Page.
   * @param contents Current contents.
   * @return New contents.
   */
  public static String fixDefaultsortCategory(Page page, String contents) {

    // Check configuration
    WPCConfiguration config = page.getWikipedia().getConfiguration();
    String option = config.getString(WPCConfigurationString.AUTO_CR_DEFAULTSORT_CATEGORY);
    if (!isValidCrOption(option)) {
      return contents;
    }
    int min = getMinCrOption(option);
    int max = getMaxCrOption(option);
    if ((min <= 0) && (max == Integer.MAX_VALUE)) {
      return contents;
    }
    PageAnalysis analysis = page.getAnalysis(contents, true);

    // Analyze each default sort
    for (PageElementFunction function : analysis.getDefaultSorts()) {

      // Count carriage returns after default sort
      int nbCr = 0;
      int index = function.getEndIndex();
      boolean finished = false;
      while (!finished && (index < contents.length())) {
        char currentChar = contents.charAt(index);
        if (currentChar == '\n') {
          nbCr++;
          index++;
        } else if (currentChar == ' ') {
          index++;
        } else {
          finished = true;
        }
      }

      // Update carriage returns after default sort
      if ((index < contents.length()) && (contents.charAt(index) == '[') &&
          ((nbCr < min) || (nbCr > max))) {
        PageElementCategory category = analysis.isInCategory(index);
        if (category != null) {
          contents = changeCarriageReturn(
              contents, function.getEndIndex(),
              normalizeValue(nbCr, min, max), category.getBeginIndex());
        }
      }
    }

    return contents;
  }

  /**
   * Insert carriage returns.
   * 
   * @param contents Contents.
   * @param begin Index where to start inserting carriage returns.
   * @param count Number of carriage returns to insert.
   * @param end Index where to end inserting carriage returns.
   * @return
   */
  private static String changeCarriageReturn(
      String contents, int begin, int count, int end) {
    StringBuilder sb = new StringBuilder(contents.substring(0, begin));
    for (int i = 0; i < count; i++) {
      sb.append('\n');
    }
    sb.append(contents.substring(end));
    return sb.toString();
  }

  /**
   * @param option Option for number of carriage returns.
   * @return True if option is valid.
   */
  private static boolean isValidCrOption(String option) {
    if ((option == null) || (option.length() == 0)) {
      return false;
    }
    int minusIndex = option.indexOf('-');
    try {
      if (minusIndex < 0) {
        int value = Integer.parseInt(option);
        if (value < 0) {
          return false;
        }
      } else {
        if (minusIndex + 1 >= option.length()) {
          return false;
        }
        int min = Integer.parseInt(option.substring(0, minusIndex));
        int max = Integer.parseInt(option.substring(minusIndex + 1));
        if ((min < 0) || (max < min)) {
          return false;
        }
      }
    } catch (NumberFormatException e) {
      return false;
    }
    return true;
  }

  /**
   * @param option Option for number of carriage returns.
   * @return Minimum number of carriage returns.
   */
  private static int getMinCrOption(String option) {
    if ((option == null) || (option.length() == 0)) {
      return 0;
    }
    int minusIndex = option.indexOf('-');
    if (minusIndex < 0) {
      return Integer.parseInt(option);
    }
    return Integer.parseInt(option.substring(0, minusIndex));
  }

  /**
   * @param option Option for number of carriage returns.
   * @return Maximum number of carriage returns.
   */
  private static int getMaxCrOption(String option) {
    if ((option == null) || (option.length() == 0)) {
      return Integer.MAX_VALUE;
    }
    int minusIndex = option.indexOf('-');
    if (minusIndex < 0) {
      return Integer.parseInt(option);
    }
    return Integer.parseInt(option.substring(minusIndex + 1));
  }

  /**
   * @param value Current value.
   * @param min Minimum possible value.
   * @param max Maximum possible value.
   * @return Normalized value.
   */
  private static int normalizeValue(int value, int min, int max) {
    if (value < min) {
      return min;
    }
    if (value > max) {
      return max;
    }
    return value;
  }

}
