/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2021  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.data.contents.magicword;

import java.util.HashMap;
import java.util.Map;

import javax.annotation.Nonnull;

/**
 * Base class for the definition of a magic word type.
 */
public abstract class MagicWordType {

  /** Map of all known magic word types by name */
  private static final Map<String, MagicWordType> mapMagicWordByName = new HashMap<>();

  /** Name */
  @Nonnull
  protected final String name;

  /** True if the magic word can be used in images */
  private final boolean isImage;

  /** True if the magic word is a function */
  private final boolean isFunction;

  /** True if the magic word requires a # to be used */
  private final boolean isWithSharp;

  /** True if the magic word changes nothing when parsing in PST mode */
  private final boolean isWithoutPST;

  /** Computed hash code */
  private final int hashCode;

  static {
    // Explicit registration of magic word types to avoid using MagicWordType before registration
    FunctionMagicWordType.registerMagicWordTypes();
    ImageMagicWordType.registerMagicWordTypes();
    SimpleMagicWordType.registerMagicWordTypes();
  }

  /**
   * @param name Name of the magic word type.
   * @return Magic word type matching the name.
   */
  @Nonnull
  public static MagicWordType getByName(@Nonnull String name) {
    MagicWordType result = mapMagicWordByName.get(name);
    if (result != null) {
      return result;
    }
    return new UnknownMagicWordType(name);
  }

  /**
   * Create a magic word type.
   * 
   * @param name Name of the magic word type.
   * @param isImage True if the magic word can be used in images.
   * @param isFunction True if the magic word is a function.
   * @param isWithSharp True if the magic word requires a # to be used.
   * @param isWithoutPST True if the magic word changes nothing when parsing in PST mode.
   */
  protected MagicWordType(
      @Nonnull String name,
      boolean isImage,
      boolean isFunction,
      boolean isWithSharp,
      boolean isWithoutPST) {
    this.name = name;
    this.isImage = isImage;
    this.isFunction = isFunction;
    this.isWithSharp = isWithSharp;
    this.isWithoutPST = isWithoutPST;
    this.hashCode = name.hashCode();
    mapMagicWordByName.put(name, this);
  }

  /**
   * @return Name of the magic word type.
   */
  @Nonnull
  public String getName() {
    return name;
  }

  /**
   * @param acceptEmpty True if empty placeholder is OK.
   * @return Pattern for placeholder.
   */
  public String getPattern(boolean acceptEmpty) {
    return acceptEmpty ? ".*" : ".+";
  }

  /**
   * @return True if the magic word type can be used in images.
   */
  public boolean isImage() {
    return isImage;
  }

  /**
   * @return True if the magic word type is a function.
   */
  public boolean isFunction() {
    return isFunction;
  }

  /**
   * @return True if the magic word requires a # to be used.
   */
  public boolean isWithSharp() {
    return isWithSharp;
  }

  /**
   * @return True if magic word changes nothing when parsing in PST mode.
   */
  public boolean isWithoutPST() {
    return isWithoutPST;
  }

  /**
   * @see java.lang.Object#hashCode()
   */
  @Override
  public int hashCode() {
    return hashCode;
  }

  /**
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
