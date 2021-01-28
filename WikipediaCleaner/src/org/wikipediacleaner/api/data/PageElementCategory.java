/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.data;

import org.wikipediacleaner.api.constants.EnumCaseSensitiveness;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.contents.ContentsUtil;
import org.wikipediacleaner.utils.string.CharacterUtils;


/**
 * Class containing information about a category link ([[category:name|sort]]). 
 */
public class PageElementCategory extends PageElement {

  public final static String DEFAULT_NAME = "Category";

  private final String categoryNotTrimmed;
  private final String category;
  private final String nameNotTrimmed;
  private final String name;
  private final String sortNotTrimmed;
  private final String sort;

  /**
   * Analyze contents to check if it matches a language link.
   * 
   * @param wikipedia Wikipedia.
   * @param contents Contents.
   * @param index Block start index.
   * @return Block details it there's a block.
   */
  public static PageElementCategory analyzeBlock(
      EnumWikipedia wikipedia, String contents, int index) {
    // Verify arguments
    if (contents == null) {
      return null;
    }

    // Look for '[['
    int tmpIndex = index;
    if ((tmpIndex >= contents.length()) ||
        (!contents.startsWith("[[", tmpIndex))) {
      return null;
    }
    tmpIndex += 2;
    int beginIndex = tmpIndex;

    // Possible white spaces characters
    tmpIndex = ContentsUtil.moveIndexAfterWhitespace(contents, tmpIndex);

    // Search for :
    tmpIndex = ContentsUtil.moveIndexForwardWhileNotFound(contents, tmpIndex, ":|][");
    if ((tmpIndex >= contents.length()) || (contents.charAt(tmpIndex) != ':')) {
      return null;
    }

    // Check that name space is a category
    int colonIndex = tmpIndex;
    Namespace categoryNS = wikipedia.getWikiConfiguration().getNamespace(Namespace.CATEGORY);
    if (!categoryNS.isPossibleName(contents.substring(beginIndex, colonIndex).trim())) {
      return null;
    }

    // Search for |
    tmpIndex = ContentsUtil.moveIndexForwardWhileNotFound(contents, tmpIndex, "|]");
    if (tmpIndex >= contents.length()) {
      return null;
    }
    String categoryName = contents.substring(colonIndex + 1, tmpIndex);

    // Simple language tag [[lang:link]]
    if (contents.charAt(tmpIndex) == ']') {
      if (!contents.startsWith("]]", tmpIndex)) {
        return null;
      }
      return new PageElementCategory(
          index, tmpIndex + 2,
          contents.substring(beginIndex, colonIndex),
          categoryName, categoryNS.getCaseSensitiveness(), null);
    }

    // Find elements of image
    int endIndex = contents.indexOf("]]", colonIndex);
    if (endIndex < 0) {
      return null;
    }
    return new PageElementCategory(
        index, endIndex + 2,
        contents.substring(beginIndex, colonIndex),
        categoryName, categoryNS.getCaseSensitiveness(),
        contents.substring(tmpIndex + 1, endIndex));
  }

  public String getCategory() {
    return category;
  }

  public String getCategoryNotTrimmed() {
    return categoryNotTrimmed;
  }

  public String getName() {
    return name;
  }

  public String getNameNotTrimmed() {
    return nameNotTrimmed;
  }

  public String getSort() {
    return sort;
  }

  public String getSortNotTrimmed() {
    return sortNotTrimmed;
  }

  private PageElementCategory(
      int beginIndex, int endIndex,
      String category, String name,
      EnumCaseSensitiveness caseSensitive,
      String sort) {
    super(beginIndex, endIndex);
    this.categoryNotTrimmed = category;
    this.category = caseSensitive.normalize(CharacterUtils.trim(category));
    this.nameNotTrimmed = name;
    this.name = CharacterUtils.trim(name);
    this.sortNotTrimmed = sort;
    this.sort = CharacterUtils.trim(sort);
  }

  /* (non-Javadoc)
   * @see java.lang.Object#toString()
   */
  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("[[");
    sb.append(categoryNotTrimmed);
    sb.append(':');
    if (nameNotTrimmed != null) {
      sb.append(nameNotTrimmed);
    }
    if (sortNotTrimmed != null) {
      sb.append('|');
      sb.append(sortNotTrimmed);
    }
    sb.append("]]");
    return sb.toString();
  }

  /**
   * Create a category.
   * 
   * @param wiki Wiki.
   * @param category Category name.
   * @param sortKey Sort key.
   * @return Category.
   */
  public static String createCategory(
      EnumWikipedia wiki,
      String category, String sortKey) {
    StringBuilder sb = new StringBuilder();
    sb.append("[[");
    Namespace categoryNamespace = wiki.getWikiConfiguration().getNamespace(Namespace.CATEGORY);
    sb.append(categoryNamespace.getTitle());
    sb.append(":");
    sb.append(category);
    if (sortKey != null) {
      sb.append("|");
      sb.append(sortKey.trim());
    }
    sb.append("]]");
    return sb.toString();
  }
}
