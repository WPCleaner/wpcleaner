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

  /** Replacements */
  private List<AutomaticFixing> replacements;

  /** Comment */
  private String comment;

  /** Additional algorithms */
  private List<Integer> additionalAlgorithms;

  /** Force algorithms */
  private List<Integer> forceAlgorithms;

  /**
   * @param replacements Replacements
   */
  @XmlElementWrapper(name = "replacements")
  @XmlElement(name = "replacement")
  public void setReplacements(List<AutomaticFixing> replacements) {
    this.replacements = replacements;
  }

  /**
   * @param comment Comment
   */
  @XmlElement(name = "comment")
  public void setComment(String comment) {
    this.comment = comment;
  }

  /**
   * @param algorithms Additional algorithms.
   */
  @XmlElementWrapper(name = "additionalAlgorithms")
  @XmlElement(name = "algorithm")
  public void setAdditionalAlgorithms(List<Integer> algorithms) {
    this.additionalAlgorithms = algorithms;
  }

  /**
   * @param algorithms Force algorithms.
   */
  @XmlElementWrapper(name = "forceAlgorithms")
  @XmlElement(name = "algorithm")
  public void setForceAlgorithms(List<Integer> algorithms) {
    this.forceAlgorithms = algorithms;
  }

  /**
   * @return Replacements.
   */
  public List<AutomaticFixing> getReplacements() {
    return this.replacements;
  }

  /**
   * @return Comment.
   */
  public String getComment() {
    return comment;
  }

  /**
   * @return Additional algorithms.
   */
  public List<Integer> getAdditionalAlgorithms() {
    return additionalAlgorithms;
  }

  /**
   * @return Force algorithms.
   */
  public List<Integer> getForceAlgorithms() {
    return forceAlgorithms;
  }
}
