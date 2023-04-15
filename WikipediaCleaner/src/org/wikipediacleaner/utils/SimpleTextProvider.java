/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2023  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.utils;

import java.util.Collection;
import java.util.Collections;
import java.util.List;


public class SimpleTextProvider implements TextProvider {

  private final List<String> texts;

  public SimpleTextProvider(final String text) {
    this.texts = Collections.singletonList(text);
  }

  @Override
  public Collection<String> getTexts() {
    return texts;
  }
}
