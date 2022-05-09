/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2022  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.check.algorithm.a0xx.a05x.a055;

import org.wikipediacleaner.api.data.PageElementTag;

class AnalysisSituation {

  public int level;
  public boolean previousUnclosedTag;
  public PageElementTag level0Tag;
  
  public AnalysisSituation() {
    this.level = 0;
    this.previousUnclosedTag = false;
    this.level0Tag = null;
  }
}
