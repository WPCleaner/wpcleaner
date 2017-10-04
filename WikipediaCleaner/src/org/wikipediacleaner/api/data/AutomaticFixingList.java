/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2017  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.data;

import java.util.List;

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElementWrapper;
import javax.xml.bind.annotation.XmlRootElement;


/**
 * List of automatic fixing parameters.
 */
@XmlRootElement(namespace = "org.wpcleaner")
public class AutomaticFixingList {

  private List<AutomaticFixing> replacements;

  @XmlElementWrapper(name = "replacements")
  @XmlElement(name = "replacement")
  public void setReplacements(List<AutomaticFixing> replacements) {
    this.replacements = replacements;
  }

  public List<AutomaticFixing> getReplacements() {
    return this.replacements;
  }
}
