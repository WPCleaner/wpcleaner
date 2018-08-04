/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.data;

import java.util.Comparator;


/**
 * A Comparator&lt;T&gt; extension for named comparators.
 */
public interface NamedComparator<T> extends Comparator<T> {

  public String getName();
}
