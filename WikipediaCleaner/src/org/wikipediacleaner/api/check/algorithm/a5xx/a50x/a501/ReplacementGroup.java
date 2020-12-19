/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2020  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.check.algorithm.a5xx.a50x.a501;

import java.util.ArrayList;
import java.util.List;

/**
 * Utility class to group possible replacements.
 */
class ReplacementGroup {

  /** Minimum index for the group of replacements. */
  private final int begin;

  /** Maximum index for the group of replacements. */
  private final int end;

  /** Original text. */
  private final String text;

  /** Group of replacements. */
  private final List<Replacement> group;

  /**
   * Create a group of replacements.
   * 
   * @param replacements Ordered list of replacements.
   * @param contents Page contents.
   */
  public ReplacementGroup(List<Replacement> replacements, String contents) {

    // Compute minimum/maximum indexes for the group of replacements.
    int minBegin = Integer.MAX_VALUE;
    int maxEnd = 0;
    for (Replacement replacement : replacements) {
      minBegin = Math.min(minBegin, replacement.getBegin());
      maxEnd = Math.max(maxEnd, replacement.getEnd());
    }
    begin = minBegin;
    end = maxEnd;
    text = contents.substring(begin, end);

    // Compute new replacements using the full expand of the group.
    List<String> alreadyAdded = new ArrayList<>();
    group = new ArrayList<>();
    for (Replacement replacement : replacements) {
      if ((replacement.getBegin() == begin) &&
          (replacement.getEnd() == end)) {
        String newText = replacement.getReplacement();
        if (!alreadyAdded.contains(newText)) {
          group.add(replacement);
          alreadyAdded.add(newText);
        }
      } else {
        String newText =
            contents.substring(begin, replacement.getBegin()) +
            replacement.getReplacement() +
            contents.substring(replacement.getEnd(), end);
        if (!alreadyAdded.contains(newText)) {
          group.add(
              new Replacement(
                  begin, end, replacement.getComment(),replacement.isOtherPattern(),
                  newText, replacement.isAutomatic()));
          alreadyAdded.add(newText);
        }
      }
    }
  }

  /**
   * @return Minimum index for the group of replacements.
   */
  public int getBegin() {
    return begin;
  }

  /**
   * @return Maximum index for the group of replacements.
   */
  public int getEnd() {
    return end;
  }

  /**
   * @return Original text.
   */
  public String getText() {
    return text;
  }

  /**
   * @return List of possible replacements.
   */
  public List<Replacement> getReplacements() {
    return group;
  }
}
