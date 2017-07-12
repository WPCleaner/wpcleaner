/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2017  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.data;

import org.wikipediacleaner.api.constants.WikiConfiguration;


/**
 * Information about Linter categories.
 */
public class LinterCategory implements Comparable<LinterCategory> {

  /** Level of the category */
  private final String level;

  /** Name of the category */
  private final String category;

  public final static String LEVEL_ERROR = "errors";
  public final static String LEVEL_WARNING = "warnings";

  /**
   * @param level Level of the category.
   * @param category Name of the category.
   */
  public LinterCategory(String level, String category) {
    this.level = level;
    this.category = category;
  }

  /**
   * @return Level of the category.
   */
  public String getLevel() {
    return level;
  }

  /**
   * @param config Wiki configuration.
   * @return Localized name of the level.
   */
  public String getLevelName(WikiConfiguration config) {
    if (config != null) {
      String result = config.getMessageByName("linter-heading-" + level + "-priority");
      if (result != null) {
        return result;
      }
    }
    return level;
  }

  /**
   * @return Name of the category.
   */
  public String getCategory() {
    return category;
  }

  /**
   * @param config Wiki configuration.
   * @return Localized name of the category.
   */
  public String getCategoryName(WikiConfiguration config) {
    if (config != null) {
      String result = config.getMessageByName("linter-category-" + category);
      if (result != null) {
        return result;
      }
    }
    return category;
  }

  /* (non-Javadoc)
   * @see java.lang.Object#equals(java.lang.Object)
   */
  @Override
  public int compareTo(LinterCategory lc) {
    int compare;

    // Level
    compare = level.compareTo(lc.level);
    if (compare != 0) {
      if (level.equals(LEVEL_ERROR)) {
        return -1;
      } else if (lc.level.equals(LEVEL_ERROR)) {
        return 1;
      }
      if (level.equals(LEVEL_WARNING)) {
        return -1;
      } else if (lc.level.equals(LEVEL_WARNING)) {
        return 1;
      }
      return compare;
    }

    // Name
    compare = category.compareTo(lc.category);
    if (compare != 0) {
      return compare;
    }

    return compare;
  }

  /* (non-Javadoc)
   * @see java.lang.Object#equals(java.lang.Object)
   */
  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if ((o == null) || (o.getClass() != getClass())) {
      return false;
    }
    LinterCategory lc = (LinterCategory) o;
    boolean equals = true;
    equals &= level.equals(lc.level);
    equals &= category.equals(lc.category);
    return equals;
  }

  /* (non-Javadoc)
   * @see java.lang.Object#hashCode()
   */
  @Override
  public int hashCode() {
    return category.hashCode();
  }
}
