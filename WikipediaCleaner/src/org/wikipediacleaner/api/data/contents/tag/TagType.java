/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2021  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.data.contents.tag;

import java.util.HashMap;
import java.util.Map;

import javax.annotation.Nonnull;

import org.apache.commons.lang3.StringUtils;

/**
 * Base class for the definition of a tag type.
 */
public abstract class TagType {

  /** Map of all known tag types by normalized name */
  private static final Map<String, TagType> mapTagByNormalizedName = new HashMap<>();

  /** Map of all known tag types by lower case name */
  private static final Map<String, TagType> mapTagByLowerCaseName = new HashMap<>();

  /** Normalized name */
  @Nonnull
  protected final String normalizedName;

  /** True if the tag can be open like <code>&lt;name&gt;</code> */
  protected final boolean openPossible;

  /** True if the tag can be close like <code>&lt;/name&gt;</code> */
  protected final boolean closePossible;

  /** True if the tag can be close like <code>&lt;name/&gt;</code> */
  protected final boolean fullPossible;

  /** True if an open tag can normally be without a close tag */
  protected final boolean unclosedOk;

  /** Simple open tag */
  private final String openTag;

  /** Simple close tag */
  private final String closeTag;

  /** Simple full tag */
  private final String fullTag;

  /** Simple complete tag */
  private final String completeTag;

  /** Computed hash code */
  private final int hashCode;

  static {
    // Explicit registration of tag types to avoid using TagType before registration
    HtmlTagType.registerTagTypes();
    OtherTagType.registerTagTypes();
    WikiTagType.registerTagTypes();
  }

  /**
   * @param name Normalized name of the tag type.
   * @return Tag type matching the normalized name.
   */
  @Nonnull
  public static TagType getByNormalizedName(@Nonnull String name) {
    TagType result = mapTagByNormalizedName.get(name);
    if (result != null) {
      return result;
    }
    return new UnknownTagType(name);
  }

  /**
   * @param name Name of the tag type.
   * @return Tag type matching the name.
   */
  @Nonnull
  public static TagType getByUncleanedName(@Nonnull String name) {
    TagType result = mapTagByLowerCaseName.get(name.toLowerCase());
    if (result != null) {
      return result;
    }
    return new UnknownTagType(name);
  }

  /**
   * Create a tag type.
   * 
   * @param name Normalized name of the tag type.
   * @param openPossible True if the tag can be open.
   * @param closePossible True if the tag can be close.
   * @param fullPossible True if the tag can be full.
   * @param unclosedOk True if an open tag can normally be without a close tag.
   */
  protected TagType(
      @Nonnull String name,
      boolean openPossible,
      boolean closePossible,
      boolean fullPossible,
      boolean unclosedOk) {
    this.normalizedName = name;
    this.hashCode = normalizedName.hashCode();
    this.openPossible = openPossible;
    this.closePossible = closePossible;
    this.fullPossible = fullPossible;
    this.unclosedOk = unclosedOk;
    this.openTag = TagBuilder.from(this, TagFormat.OPEN).toString();
    this.closeTag = TagBuilder.from(this, TagFormat.CLOSE).toString();
    this.fullTag = TagBuilder.from(this, TagFormat.FULL).toString();
    this.completeTag = CompleteTagBuilder.from(this, "...").toString();
    mapTagByNormalizedName.put(name, this);
    mapTagByLowerCaseName.put(name.toLowerCase(), this);
  }

  /**
   * @return Normalized name of the tag type.
   */
  @Nonnull
  public String getNormalizedName() {
    return normalizedName;
  }

  /**
   * @param name Name of a tag.
   * @return True if the name of a tag matches the tag type.
   */
  public boolean isPossibleName(String name) {
    return StringUtils.equalsIgnoreCase(normalizedName, name);
  }

  /**
   * @return True if the tag can be open like <code>&lt;name&gt;</code>.
   */
  public boolean isOpenPossible() {
    return openPossible;
  }

  /**
   * @return True if the tag can be close like <code>&lt;/name&gt;</code>.
   */
  public boolean isClosePossible() {
    return closePossible;
  }

  /**
   * @return True if the tag can be full like <code>&lt;name/&gt;</code>.
   */
  public boolean isFullPossible() {
    return fullPossible;
  }

  /**
   * @return True if an open tag can normally be without a close tag.
   */
  public boolean isUnclosedOk() {
    return unclosedOk;
  }

  /**
   * @return A textual representation of a simple open tag.
   */
  public String getOpenTag() {
    return openTag;
  }

  /**
   * @return A textual representation of a simple close tag.
   */
  public String getCloseTag() {
    return closeTag;
  }

  /**
   * @return A textual representation of a simple full tag.
   */
  public String getFullTag() {
    return fullTag;
  }

  /**
   * @return A textual representation of a simple complete tag.
   */
  public String getCompleteTag() {
    return completeTag;
  }

  /**
   * @return
   * @see java.lang.Object#hashCode()
   */
  @Override
  public int hashCode() {
    return hashCode;
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
    return false;
  }
}
