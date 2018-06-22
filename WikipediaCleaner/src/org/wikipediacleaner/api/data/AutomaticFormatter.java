/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.data;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.apache.commons.codec.digest.DigestUtils;
import org.wikipediacleaner.api.check.CheckError;
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
   * @param botFix True to use bot fixes.
   * @param usedAlgorithms Algorithms used to tidy up the article.
   * @return New contents.
   */
  public static String tidyArticle(
      Page page, String contents,
      Collection<CheckErrorAlgorithm> algorithms, boolean botFix,
      List<CheckError.Progress> usedAlgorithms) {
    if ((page == null) || (contents == null)) {
      return contents;
    }
    EnumWikipedia wiki = page.getWikipedia();
    WPCConfiguration config = wiki.getConfiguration();

    // Fix Check Wiki errors
    if (algorithms != null) {
      boolean finished = true;
      List<String> md5List = new ArrayList<>();
      md5List.add(DigestUtils.md5Hex(contents));
      int modificationsCount = 0; 
      do {
        finished = true;
        for (CheckErrorAlgorithm algorithm : algorithms) {
          if (algorithm.isAvailable() &&
              CheckErrorAlgorithms.isAlgorithmActive(wiki, algorithm.getErrorNumber())) {
            String currentContents = contents;
            boolean modified = false;
            do {
              currentContents = contents;
              PageAnalysis analysis = page.getAnalysis(currentContents, true);
              contents = botFix ? algorithm.botFix(analysis) : algorithm.automaticFix(analysis);
              if (!contents.equals(currentContents)) {
                String md5 = DigestUtils.md5Hex(contents);
                if (md5List.contains(md5)) {
                  contents = currentContents;
                } else {
                  modified = true;
                  modificationsCount++;
                  md5List.add(md5);
                }
              }
            } while (!contents.equals(currentContents) && (modificationsCount < 1000));
            if (modified) {
              finished = false;
              if (usedAlgorithms != null) {
                boolean shouldAdd = true;
                for (CheckError.Progress progress : usedAlgorithms) {
                  if (progress.algorithm == algorithm) {
                    shouldAdd = false;
                  }
                }
                if (shouldAdd) {
                  usedAlgorithms.add(new CheckError.Progress(algorithm, true));
                  // TODO: compute if fix is complete ?
                }
              }
            }
          }
        }
      } while (!finished);
    }

    // Auto formatting options
    if (!page.isInMainNamespace()) {
      return contents;
    }
    if (!config.getBoolean(WPCConfigurationBoolean.AUTO_ACTIVE)) {
      return contents;
    }
    contents = fixSpaceAroundTitle(page, contents);
    contents = fixLinkDefaultsortCategory(page, contents);
    contents = fixLangLinksAfterCategory(page, contents);
    contents = fixCrBeforeCategory(page, contents);
    contents = fixCrDefaultsortCategory(page, contents);
    contents = fixCrBetweenCategory(page, contents);
    contents = fixEndOfArticle(page, contents);

    return contents;
  }

  /**
   * Auto formatting options: link default sort and categories.
   * 
   * @param page Page.
   * @param contents Current contents.
   * @return New contents.
   */
  public static String fixLinkDefaultsortCategory(Page page, String contents) {

    // Check configuration
    WPCConfiguration config = page.getWikipedia().getConfiguration();
    boolean option = config.getBoolean(WPCConfigurationBoolean.AUTO_LINK_DEFAULTSORT_CATEGORY);
    if (!option) {
      return contents;
    }
    PageAnalysis analysis = page.getAnalysis(contents, true);

    // Retrieve default sort
    List<PageElementFunction> defaultSorts = analysis.getDefaultSorts();
    if ((defaultSorts == null) || (defaultSorts.isEmpty())) {
      return contents;
    }
    PageElementFunction defaultSort = defaultSorts.get(0);
    int beginDefaultSort = defaultSort.getBeginIndex();
    int endDefaultSort = defaultSort.getEndIndex();

    // Retrieve categories
    List<PageElementCategory> categories = analysis.getCategories();
    if ((categories == null) || (categories.isEmpty())) {
      return contents;
    }
    PageElementCategory category = categories.get(0);
    int beginCategory = category.getBeginIndex();
    boolean defaultSortFirst = beginDefaultSort < beginCategory;

    // Analyze text between category and default sort
    if (!defaultSortFirst) {
      int index = category.getEndIndex();
      while (index < beginDefaultSort) {
        char currentChar = contents.charAt(index);
        if ((currentChar == ' ') || (currentChar == '\n')) {
          index++;
        } else if (currentChar == '[') {
          PageElementCategory currentCategory = analysis.isInCategory(index);
          if (currentCategory == null) {
            return contents;
          }
          index = currentCategory.getEndIndex();
        } else {
          return contents;
        }
      }
    } else {
      int index = endDefaultSort;
      boolean ok = true;
      while (ok && (index < beginCategory)) {
        char currentChar = contents.charAt(index);
        if ((currentChar != ' ') && (currentChar != '\n')) {
          ok = false;
        }
        index++;
      }
      if (ok) {
        return contents;
      }
      index = beginCategory;
      while ((index > 0) && ok) {
        char currentChar = contents.charAt(index);
        if (currentChar == '\n') {
          ok = false;
        } else if (currentChar != ' ') {
          return contents;
        }
        index--;
      }
    }

    // Fix default sort position
    int delta = 0;
    if ((beginDefaultSort <= 0) ||
        (contents.charAt(beginDefaultSort - 1) == '\n')) {
      if ((endDefaultSort < contents.length()) &&
          (contents.charAt(endDefaultSort) == '\n')) {
        delta = 1;
        if ((beginDefaultSort <= 1) ||
            (contents.charAt(beginDefaultSort - 2) == '\n')) {
          if ((endDefaultSort + 1 < contents.length() &&
              (contents.charAt(endDefaultSort + 1) == '\n'))) {
            delta = 2;
          }
        }
      }
    }
    StringBuilder sb = new StringBuilder(contents.substring(
        0,
        defaultSortFirst ? beginDefaultSort : beginCategory));
    if (defaultSortFirst) {
      sb.append(contents.substring(endDefaultSort + delta, beginCategory));
    }
    sb.append(contents.substring(beginDefaultSort, endDefaultSort));
    sb.append("\n");
    if (defaultSortFirst) {
      if (beginCategory < contents.length()) {
        sb.append(contents.substring(beginCategory));
      }
    } else {
      sb.append(contents.substring(beginCategory, beginDefaultSort));
      if (endDefaultSort + delta < contents.length()) {
        sb.append(contents.substring(endDefaultSort + delta));
      }
    }
    contents = sb.toString();

    return contents;
  }

  /**
   * Auto formatting options: number of carriage returns before categories.
   * 
   * @param page Page.
   * @param contents Current contents.
   * @return New contents.
   */
  public static String fixCrBeforeCategory(Page page, String contents) {

    // Check configuration
    WPCConfiguration config = page.getWikipedia().getConfiguration();
    String option = config.getString(WPCConfigurationString.AUTO_CR_BEFORE_CATEGORY);
    if (!isValidCountOption(option)) {
      return contents;
    }
    int min = getMinCountOption(option);
    int max = getMaxCountOption(option);
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
      contents = changeCharacters(
          contents, index + 1, '\n',
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
    contents = changeCharacters(
        contents, index + 1, '\n',
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
  public static String fixCrDefaultsortCategory(Page page, String contents) {

    // Check configuration
    WPCConfiguration config = page.getWikipedia().getConfiguration();
    String option = config.getString(WPCConfigurationString.AUTO_CR_DEFAULTSORT_CATEGORY);
    if (!isValidCountOption(option)) {
      return contents;
    }
    int min = getMinCountOption(option);
    int max = getMaxCountOption(option);
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
          contents = changeCharacters(
              contents, function.getEndIndex(), '\n',
              normalizeValue(nbCr, min, max), category.getBeginIndex());
        }
      }
    }

    return contents;
  }

  /**
   * Auto formatting options: number of carriage returns between each category.
   * 
   * @param page Page.
   * @param contents Current contents.
   * @return New contents.
   */
  public static String fixCrBetweenCategory(Page page, String contents) {

    // Check configuration
    WPCConfiguration config = page.getWikipedia().getConfiguration();
    String option = config.getString(WPCConfigurationString.AUTO_CR_BETWEEN_CATEGORY);
    if (!isValidCountOption(option)) {
      return contents;
    }
    int min = getMinCountOption(option);
    int max = getMaxCountOption(option);
    if ((min <= 0) && (max == Integer.MAX_VALUE)) {
      return contents;
    }
    PageAnalysis analysis = page.getAnalysis(contents, true);

    // Analyze each category
    List<PageElementCategory> categories = analysis.getCategories();
    if ((categories == null) || (categories.isEmpty())) {
      return contents;
    }
    StringBuilder sb = new StringBuilder();
    int lastIndex = 0;
    for (int numCategory = 1; numCategory < categories.size(); numCategory++) {
      PageElementCategory previousCategory = categories.get(numCategory - 1);
      PageElementCategory category = categories.get(numCategory);

      // Check what is between the two categories
      int index = previousCategory.getEndIndex();
      boolean ok = true;
      int nbCr = 0;
      while (ok && (index < category.getBeginIndex())) {
        char currentChar = contents.charAt(index);
        if (currentChar == '\n') {
          nbCr++;
        } else if (currentChar != ' ') {
          ok = false;
        }
        index++;
      }

      // Update text if needed
      if (ok && ((nbCr < min) || (nbCr > max))) {
        if (lastIndex < previousCategory.getEndIndex()) {
          sb.append(contents.substring(lastIndex, previousCategory.getEndIndex()));
          lastIndex = previousCategory.getEndIndex();
        }
        nbCr = normalizeValue(nbCr, min, max);
        for (int i = 0; i < nbCr; i++) {
          sb.append('\n');
        }
        lastIndex = category.getBeginIndex();
      }
    }
    if (lastIndex > 0) {
      if (lastIndex < contents.length()) {
        sb.append(contents.substring(lastIndex));
      }
      contents = sb.toString();
    }

    return contents;
  }

  /**
   * Auto formatting options: language links after categories.
   * 
   * @param page Page.
   * @param contents Current contents.
   * @return New contents.
   */
  public static String fixLangLinksAfterCategory(Page page, String contents) {

    // Check configuration
    WPCConfiguration config = page.getWikipedia().getConfiguration();
    boolean option = config.getBoolean(WPCConfigurationBoolean.AUTO_LANGLINK_AFTER_CATEGORY);
    if (!option) {
      return contents;
    }
    PageAnalysis analysis = page.getAnalysis(contents, true);

    // Check if language links are already after categories
    List<PageElementCategory> categories = analysis.getCategories();
    if ((categories == null) || (categories.isEmpty())) {
      return contents;
    }
    List<PageElementLanguageLink> links = analysis.getLanguageLinks();
    if ((links == null) || (links.isEmpty())) {
      return contents;
    }
    if (links.get(0).getBeginIndex() >= categories.get(categories.size() - 1).getEndIndex()) {
      return contents;
    }

    // Analyze each language link
    StringBuilder sb = new StringBuilder();
    int lastIndex = 0;
    int numLink = 0;
    while (numLink < links.size()) {

      // Find first element before link
      int beginNum = numLink;
      PageElementLanguageLink begin = links.get(beginNum);
      boolean done = false;
      int beginIndex = begin.getBeginIndex();
      while ((beginIndex > 0) &&
             ((contents.charAt(beginIndex - 1) == ' ') ||
              (contents.charAt(beginIndex - 1) == '\n'))) {
        beginIndex--;
      }

      // Group language links
      done = false;
      while ((numLink + 1 < links.size()) && !done) {
        int index = links.get(numLink).getEndIndex();
        int max = links.get(numLink + 1).getBeginIndex();
        while ((index < max) && !done) {
          char current = contents.charAt(index);
          if ((current != ' ') && (current != '\n')) {
            done = true;
          }
          index++;
        }
        if (!done) {
          numLink++;
        }
      }
      int endNum = numLink;
      PageElementLanguageLink end = links.get(endNum);

      // Check if the group is before default sort/categories
      int index = end.getEndIndex();
      done = false;
      while ((index < contents.length()) && !done) {
        char current = contents.charAt(index);
        if ((current != ' ') && (current != '\n')) {
          done = true;
        } else {
          index++;
        }
      }
      PageElement firstElement = null;
      if (done) {
        char current = contents.charAt(index);
        if (current == '{') {
          firstElement = analysis.isInDefaultSort(index);
        } else if (current == '[') {
          firstElement = analysis.isInCategory(index);
        }
      }

      // Group default sort/categories
      if (firstElement != null) {
        index = firstElement.getEndIndex();
        PageElement lastElement = firstElement;
        done = false;
        while ((index < contents.length()) && !done) {
          char current = contents.charAt(index);
          if ((current == ' ') || (current == '\n')) {
            index++;
          } else {
            PageElement element = null;
            if (current == '{') {
              element = analysis.isInDefaultSort(index);
            } else if (current == '[') {
              element = analysis.isInCategory(index);
            }
            if (element != null) {
              lastElement = element;
              index = element.getEndIndex();
            } else {
              done = true;
            }
          }
        }

        // Modify contents
        if (lastIndex < begin.getBeginIndex()) {
          sb.append(contents.substring(lastIndex, beginIndex));
          lastIndex = beginIndex;
        }
        sb.append(contents.substring(end.getEndIndex(), lastElement.getEndIndex()));
        sb.append(contents.substring(beginIndex, end.getEndIndex()));
        lastIndex = lastElement.getEndIndex();
      }

      numLink++;
    }
    if (lastIndex > 0) {
      if (lastIndex < contents.length()) {
        sb.append(contents.substring(lastIndex));
      }
      contents = sb.toString();
    }

    return contents;
  }

  /**
   * Auto formatting options: end of article.
   * 
   * @param page Page.
   * @param contents Current contents.
   * @return New contents.
   */
  public static String fixEndOfArticle(Page page, String contents) {
    int index = contents.length();
    int nbCr = 0;
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
    if (index < contents.length() - 2) {
      if (nbCr >= 1) {
        return contents.substring(0, index + 1) + "\n";
      }
      return contents.substring(0, index + 1);
    }
    return contents;
  }

  /**
   * Auto formatting options: number of space characters around titles.
   * 
   * @param page Page.
   * @param contents Current contents.
   * @return New contents.
   */
  public static String fixSpaceAroundTitle(Page page, String contents) {

    // Check configuration
    WPCConfiguration config = page.getWikipedia().getConfiguration();
    String option = config.getString(WPCConfigurationString.AUTO_SPACE_AROUND_TITLE);
    if (!isValidCountOption(option)) {
      return contents;
    }
    int min = getMinCountOption(option);
    int max = getMaxCountOption(option);
    if ((min <= 0) && (max == Integer.MAX_VALUE)) {
      return contents;
    }
    PageAnalysis analysis = page.getAnalysis(contents, true);

    // Analyze each title
    List<PageElementTitle> titles = analysis.getTitles();
    if ((titles == null) || (titles.isEmpty())) {
      return contents;
    }
    StringBuilder sb = new StringBuilder();
    int lastIndex = 0;
    for (PageElementTitle title : titles) {
      String titleValue = title.getTitleNotTrimmed();
      String titleAfter = title.getAfterTitle();
      if ((titleValue != null) &&
          ((titleAfter == null) || (titleAfter.equals(""))) &&
          (title.getFirstLevel() == title.getSecondLevel())) {

        // Count space characters
        int nbSpaceBefore = 0;
        while ((nbSpaceBefore < titleValue.length()) &&
               (" \u00A0".indexOf(titleValue.charAt(nbSpaceBefore)) >= 0)) {
          nbSpaceBefore++;
        }
        int nbSpaceAfter = 0;
        while ((nbSpaceAfter < titleValue.length()) &&
               (" \u00A0".indexOf(titleValue.charAt(titleValue.length() - nbSpaceAfter - 1)) >= 0)) {
          nbSpaceAfter++;
        }

        // Update title if needed
        if ((nbSpaceBefore < min) || (nbSpaceBefore > max) ||
            (nbSpaceAfter < min) || (nbSpaceAfter > max)) {
          if (lastIndex < title.getBeginIndex()) {
            sb.append(contents.substring(lastIndex, title.getBeginIndex()));
            lastIndex = title.getBeginIndex();
          }
          nbSpaceBefore = normalizeValue(nbSpaceBefore, min, max);
          nbSpaceAfter = normalizeValue(nbSpaceAfter, min, max);
          StringBuilder newTitle = new StringBuilder();
          for (int i = 0; i < nbSpaceBefore; i++) {
            newTitle.append(' ');
          }
          newTitle.append(titleValue.trim());
          for (int i = 0; i < nbSpaceAfter; i++) {
            newTitle.append(' ');
          }
          sb.append(PageElementTitle.createUntrimmedTitle(title.getLevel(), newTitle.toString(), titleAfter));
          lastIndex = title.getEndIndex();
        }
      }
    }
    if (lastIndex > 0) {
      if (lastIndex < contents.length()) {
        sb.append(contents.substring(lastIndex));
      }
      contents = sb.toString();
    }

    return contents;
  }

  // Utility functions

  /**
   * Insert characters.
   * 
   * @param contents Contents.
   * @param begin Index where to start inserting characters.
   * @param character Character to insert.
   * @param count Number of characters to insert.
   * @param end Index where to end inserting characters.
   * @return Modified string.
   */
  private static String changeCharacters(
      String contents, int begin, char character, int count, int end) {
    StringBuilder sb = new StringBuilder(contents.substring(0, begin));
    for (int i = 0; i < count; i++) {
      sb.append(character);
    }
    sb.append(contents.substring(end));
    return sb.toString();
  }

  /**
   * @param option Option for number of characters.
   * @return True if option is valid.
   */
  private static boolean isValidCountOption(String option) {
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
   * @param option Option for number of characters.
   * @return Minimum number of characters.
   */
  private static int getMinCountOption(String option) {
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
   * @param option Option for number of characters.
   * @return Maximum number of characters.
   */
  private static int getMaxCountOption(String option) {
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
