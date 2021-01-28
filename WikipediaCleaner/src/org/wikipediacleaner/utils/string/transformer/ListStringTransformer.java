/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2021  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.utils.string.transformer;

import java.util.Arrays;
import java.util.List;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

/**
 * A String transformer that applies a list of String transformers.
 */
public class ListStringTransformer implements StringTransformer {

  @Nonnull
  private final List<StringTransformer> transformers;

  /**
   * Constructor.
   * 
   * @param List of transformers to apply.
   */
  public ListStringTransformer(@Nonnull StringTransformer... transformers) {
    this.transformers = Arrays.asList(transformers);
  }

  /**
   * @param original Original String.
   * @return Transformed String.
   * @see org.wikipediacleaner.utils.string.transformer.StringTransformer#transform(java.lang.String)
   */
  @Override
  public String transform(@Nullable String original) {
    if (original == null) {
      return original;
    }
    for (StringTransformer transformer : transformers) {
      original = transformer.transform(original);
    }
    return original;
  }

}
