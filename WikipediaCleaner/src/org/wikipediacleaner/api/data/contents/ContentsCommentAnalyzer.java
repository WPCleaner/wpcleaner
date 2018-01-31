/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2018  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.data.contents;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;


/**
 * Analyzer for comments.
 */
public class ContentsCommentAnalyzer extends ContentsElementAnalyzer {

  /**
   * Analyzer for comments.
   */
  public ContentsCommentAnalyzer() {
    super(Arrays.asList(new Class[] { ContentsComment.class }));
  }

  /**
   * @param contents
   * @see org.wikipediacleaner.api.data.contents.ContentsElementAnalyzer#analyze(org.wikipediacleaner.api.data.contents.Contents)
   */
  @Override
  void analyze(Contents contents) {
    if (contents.comments != null) {
      return;
    }

    // Analyze each comment
    boolean done = false;
    int beginIndex = 0;
    List<ContentsComment> comments = new ArrayList<>();
    do {
      beginIndex = contents.getText().indexOf(ContentsComment.START, beginIndex);
      if (beginIndex >= 0) {
        int endIndex = contents.getText().indexOf(ContentsComment.END, beginIndex);
        if (endIndex >= 0) {
          endIndex += ContentsComment.END.length();
          comments.add(new ContentsComment(
              contents, new ContentsInterval(beginIndex, endIndex)));
          beginIndex = endIndex;
        } else {
          done = true;
        }
      } else {
        done = true;
      }
    } while (!done);
    contents.comments = new ContentsCommentContainer(comments);
  }
}
