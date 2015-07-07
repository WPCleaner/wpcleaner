/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.data;

import java.util.LinkedList;

import org.wikipediacleaner.api.constants.EnumCaseSensitiveness;


/**
 * Information about namespaces.
 */
public class Namespace implements Comparable<Namespace> {

  public final static int MAIN = 0;
  public final static int MAIN_TALK = 1;
  public final static int USER = 2;
  public final static int USER_TALK = 3;
  public final static int WIKIPEDIA = 4;
  public final static int WIKIPEDIA_TALK = 5;
  public final static int IMAGE = 6;
  public final static int IMAGE_TALK = 7;
  public final static int MEDIAWIKI = 8;
  public final static int MEDIAWIKI_TALK = 9;
  public final static int TEMPLATE = 10;
  public final static int TEMPLATE_TALK = 11;
  public final static int HELP = 12;
  public final static int HELP_TALK = 13;
  public final static int CATEGORY = 14;
  public final static int CATEGORY_TALK = 15;

  private final Integer id;
  private final String title;
  private final String canonicalTitle;
  private final EnumCaseSensitiveness caseSensitiveness;
  private final boolean subPages;
  private final LinkedList<String> aliases;

  /**
   * @param id Namespace Id.
   * @param title Namespace title.
   * @param canonicalTitle Canonical title.
   * @param caseSensitiveness Case sensitiveness.
   * @param subPages True if sub pages are allowed.
   */
  public Namespace(
      String id, String title, String canonicalTitle,
      EnumCaseSensitiveness caseSensitiveness, boolean subPages) {
    Integer tmpId = null;
    try {
      tmpId = Integer.parseInt(id);
    } catch (NumberFormatException e) {
      tmpId = Integer.valueOf(-1);
    }
    this.id = tmpId;
    this.title = title;
    this.canonicalTitle = canonicalTitle;
    this.caseSensitiveness = caseSensitiveness;
    this.subPages = subPages;
    this.aliases = new LinkedList<String>();
    addAlias(this.title);
    addAlias(this.canonicalTitle);
  }

  /**
   * @return Namespace id.
   */
  public Integer getId() {
    return id;
  }

  /**
   * @return Title.
   */
  public String getTitle() {
    return title;
  }

  /**
   * @return Canonical title.
   */
  public String getCanonicalTitle() {
    return canonicalTitle;
  }

  /**
   * @return Case sensitiveness.
   */
  public EnumCaseSensitiveness getCaseSensitiveness() {
    return caseSensitiveness;
  }

  /**
   * @return True if sub pages are allowed.
   */
  public boolean areSubPagesAllowed() {
    return subPages;
  }

  /**
   * @return Aliases.
   */
  public LinkedList<String> getAliases() {
    return aliases;
  }
  
  /**
   * @param alias Alias to be added.
   */
  public void addAlias(String alias) {
    if (alias == null) {
      return;
    }
    alias = Page.getStringUcFirst(alias);
    if (!aliases.contains(alias)) {
      aliases.add(alias);
    }
  }

  /**
   * @param name Namespace name.
   * @return Flag indicating if the given name can represent this namespace.
   */
  public boolean isPossibleName(String name) {
    if (name == null) {
      return false;
    }
    name = Page.getStringUcFirst(name.trim());
    for (String alias : aliases) {
      if (name.equals(alias)) {
        return true;
      }
    }
    return false;
  }

  /* (non-Javadoc)
   * @see java.lang.Object#toString()
   */
  @Override
  public String toString() {
    return "" + id + " - " + title;
  }

  /* (non-Javadoc)
   * @see java.lang.Comparable#compareTo(java.lang.Object)
   */
  @Override
  public int compareTo(Namespace bl) {
    int compare;

    // Namespace
    compare = id.compareTo(bl.id);
    if (compare != 0) {
      return compare;
    }

    // Title
    compare = title.compareTo(bl.title);
    
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
    Namespace bl = (Namespace) o;
    boolean equals = true;
    equals &= id.equals(bl.id);
    equals &= title.equals(bl.title);
    return equals;
  }

  /* (non-Javadoc)
   * @see java.lang.Object#hashCode()
   */
  @Override
  public int hashCode() {
    int hash = 7;
    hash = 31 * hash + id.hashCode();
    hash = 31 * hash + title.hashCode();
    return hash;
  }
}
