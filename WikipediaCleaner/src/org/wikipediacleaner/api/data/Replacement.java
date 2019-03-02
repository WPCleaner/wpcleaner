/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2019  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.data;


/**
 * A bean for memorizing a possible text replacement.
 */
public final class Replacement {

  /** Replacement text */
  public final String replacement;

  /** Descriptive text for the replacement */
  public final String text;

  /** True if replacement can be automatic */
  public final boolean automatic;

  /** True if replacement can be automatic in bot mode */
  public final boolean automaticBot;

  /**
   * Constructor.
   * 
   * @param replacement Replacement text.
   */
  public Replacement(String replacement) {
    this(replacement, false);
  }

  /**
   * Constructor.
   * 
   * @param replacement Replacement text.
   * @param automatic True if replacement can be automatic.
   */
  public Replacement(String replacement, boolean automatic) {
    this(replacement, automatic, automatic);
  }

  /**
   * Constructor.
   * 
   * @param replacement Replacement text.
   * @param automatic True if replacement can be automatic.
   * @param automaticBot True if replacement can be automatic in bot mode.
   */
  public Replacement(String replacement, boolean automatic, boolean automaticBot) {
    this(replacement, null, automatic, automaticBot);
  }

  /**
   * Constructor.
   * 
   * @param replacement Replacement text.
   * @param text Descriptive text.
   */
  public Replacement(String replacement, String text) {
    this(replacement, text, false);
  }

  /**
   * Constructor.
   * 
   * @param replacement Replacement text.
   * @param text Descriptive text.
   * @param automatic True if replacement can be automatic.
   */
  public Replacement(String replacement, String text, boolean automatic) {
    this(replacement, text, automatic, automatic);
  }

  /**
   * Constructor.
   * 
   * @param replacement Replacement text.
   * @param text Descriptive text.
   * @param automatic True if replacement can be automatic.
   * @param automaticBot True if replacement can be automatic in bot mode.
   */
  public Replacement(String replacement, String text, boolean automatic, boolean automaticBot) {
    this.replacement = replacement;
    this.text = text;
    this.automatic = automatic;
    this.automaticBot = automaticBot;
  }

  /**
   * @return  a hash code value for this object.
   * @see java.lang.Object#hashCode()
   */
  @Override
  public int hashCode() {
    if (replacement != null) {
      return replacement.hashCode();
    }
    return 0;
  }

  /**
   * Compare 2 Replacement objects.
   * 
   * @param   obj   the reference object with which to compare.
   * @return  {@code true} if this object is the same as the obj
   *          argument; {@code false} otherwise.
   * @see java.lang.Object#equals(java.lang.Object)
   */
  @Override
  public boolean equals(Object obj) {

    // Check object type
    if (!(obj instanceof Replacement)) {
      return false;
    }
    Replacement r2 = (Replacement) obj;

    // Check replacement text
    if (replacement == null) {
      if (r2.replacement != null) {
        return false;
      }
    } else {
      if ((r2.replacement == null) || (!replacement.equals(r2.replacement))) {
        return false;
      }
    }

    // Check descriptive text
    if (text == null) {
      if (r2.text != null) {
        return false;
      }
    } else {
      if ((r2.text == null) || (!text.equals(r2.text))) {
        return false;
      }
    }

    // Check automatic flag
    if (automatic != r2.automatic) {
      return false;
    }
    if (automaticBot != r2.automaticBot) {
      return false;
    }

    return false;
  }

}
