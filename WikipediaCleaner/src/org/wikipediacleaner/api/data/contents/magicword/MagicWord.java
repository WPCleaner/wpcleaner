/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.data.contents.magicword;

import java.util.List;


/**
 * Information about magic words.
 */
public class MagicWord implements Comparable<MagicWord> {

  /** Magic word type. */
  private final MagicWordType type;

  /** Magic word name. */
  private final String name;

  /** List of magic word aliases. */
  private final List<String> aliases;

  /** Flag indicating if the name is case sensitive. */
  private final boolean caseSensitive;

  /** Flag indicating if the magic word is for a function. */
  private final boolean isFunction;

  /** Flag indicating if the magic word is for a function but change nothing in PST mode (PreSave Transform). */
  private final boolean isFunctionNotPST;

  /** Flag indicating if the magic word if for an image. */
  private final boolean isImage;

  /**
   * @param type Magic word type.
   * @param aliases Magic word aliases.
   * @param caseSensitive True if case sensitiveness is needed.
   */
  public MagicWord(MagicWordType type, List<String> aliases, boolean caseSensitive) {
    this.type = type;
    this.name = type.getName();
    this.aliases = aliases;
    this.caseSensitive = caseSensitive;
    this.isFunction = type.isFunction();
    this.isFunctionNotPST = type.isWithoutPST() && type.isFunction();
    this.isImage = type.isImage();
  }

  /**
   * @param name Magic word name.
   * @param aliases Magic word aliases.
   * @param caseSensitive True if case sensitiveness is needed.
   */
  public MagicWord(String name, List<String> aliases, boolean caseSensitive) {
    this(MagicWordType.getByName(name), aliases, caseSensitive);
  }

  /**
   * @return Magic word type.
   */
  public MagicWordType getType() {
    return type;
  }

  /**
   * @return Magic word name.
   */
  public String getName() {
    return name;
  }

  /**
   * @return Magic word aliases.
   */
  public List<String> getAliases() {
    return aliases;
  }

  /**
   * @param text Text to check.
   * @return Flag indicating if the text is a possible alias.
   */
  public boolean isPossibleAlias(String text) {
    return isPossibleAlias(text, true);
  }

  /**
   * @param text Text to check.
   * @param acceptEmpty True if an empty value for the placeholder is ok.
   * @return Flag indicating if the text is a possible alias.
   */
  public boolean isPossibleAlias(String text, boolean acceptEmpty) {
    String pattern = acceptEmpty ? ".*" : ".+";
    if (text == null) {
      return false;
    }
    if ((text.length() > 0) &&
        (text.charAt(0) == '#') &&
        type.isWithSharp()) {
      text = text.substring(1);
    }
    for (String alias : aliases) {
      if (alias.contains("$1")) {
        if (text.matches(alias.replaceAll("\\$1", pattern))) {
          return true;
        }
      } else if (alias.equals(text)) {
        return true;
      } else if (!caseSensitive && alias.equalsIgnoreCase(text)) {
        return true;
      }
    }
    return false;
  }

  /**
   * @return True if the magic word is for a function.
   */
  public boolean isFunctionMagicWord() {
    return isFunction;
  }

  /**
   * @return True if the magic word is for a function but change nothing in PST mode (PreSave Transform).
   */
  public boolean isFunctionNotPSTMagicWord() {
    return isFunctionNotPST;
  }

  /**
   * @return True if the magic word is for an image.
   */
  public boolean isImageMagicWord() {
    return isImage;
  }

  /* (non-Javadoc)
   * @see java.lang.Comparable#compareTo(java.lang.Object)
   */
  @Override
  public int compareTo(MagicWord mw) {
    int compare;

    // Name
    compare = name.compareTo(mw.name);
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
    MagicWord mw = (MagicWord) o;
    boolean equals = true;
    equals &= name.equals(mw.name);
    return equals;
  }

  /* (non-Javadoc)
   * @see java.lang.Object#hashCode()
   */
  @Override
  public int hashCode() {
    return name.hashCode();
  }
}
