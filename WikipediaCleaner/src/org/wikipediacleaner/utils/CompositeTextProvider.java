/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2023  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.utils;

import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;


public class CompositeTextProvider implements TextProvider {

  private final List<TextProvider> providers;
  
  public CompositeTextProvider(final List<TextProvider> providers) {
    this.providers = providers;
  }

  /**
   * @return
   * @see org.wikipediacleaner.utils.TextProvider#getTexts()
   */
  @Override
  public Collection<String> getTexts() {
    return providers.stream().map(TextProvider::getTexts).flatMap(Collection::stream).collect(Collectors.toList());
  }
}
