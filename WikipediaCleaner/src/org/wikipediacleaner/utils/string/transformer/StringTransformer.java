/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2021  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.utils.string.transformer;


/**
 * A generic String transformer (takes a String as an input and returns a transformed String).
 */
@FunctionalInterface
public interface StringTransformer {

  /**
   * Transform a String.
   * 
   * @param original Original String.
   * @return Transformed String.
   */
  String transform(String original);
}
