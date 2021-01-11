/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2021  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.data.contents.tag;

import javax.annotation.Nonnull;

/**
 * Definition of an unknown tag type.
 */
class UnknownTagType extends TagType {

  /**
   * @param name Normalized name of the tag type.
   */
  public UnknownTagType(@Nonnull String name) {
    super(name.toLowerCase(), true, true, true, true);
  }

  /**
   * @param obj
   * @return
   * @see java.lang.Object#equals(java.lang.Object)
   */
  @Override
  public boolean equals(Object obj) {
    if (this == obj) {
      return true;
    }
    if (obj == null) {
      return false;
    }
    if (getClass() != obj.getClass()) {
      return false;
    }
    return normalizedName.equals(obj);
  }
}
