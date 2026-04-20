package org.wikipediacleaner.gui.swing.bot.listcw;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithm;
import org.wikipediacleaner.api.data.Page;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Bean for holding information about processing for an algorithm.
 */
class AlgorithmInformation {

  /**
   * Algorithm.
   */
  final CheckErrorAlgorithm algorithm;

  /**
   * Errors found.
   */
  private final Map<String, Detection> detections;

  /**
   * Time spent in analysis.
   */
  private long timeSpent;

  /**
   * @param algorithm Algorithm.
   */
  private AlgorithmInformation(CheckErrorAlgorithm algorithm) {
    this.algorithm = algorithm;
    this.detections = new HashMap<>();
    this.timeSpent = 0;
  }

  /**
   * Create a list for information about processing algorithms.
   *
   * @param algorithms List of algorithms.
   * @return List of information initialized.
   */
  public static List<AlgorithmInformation> createList(List<CheckErrorAlgorithm> algorithms) {
    List<AlgorithmInformation> list = new ArrayList<>(algorithms.size());
    for (CheckErrorAlgorithm algorithm : algorithms) {
      list.add(new AlgorithmInformation(algorithm));
    }
    return list;
  }

  /**
   * @return Errors found.
   */
  public Map<String, Detection> getDetections() {
    return detections;
  }

  /**
   * @param page   Page.
   * @param errors List of errors.
   */
  public void addDetection(Page page, List<CheckErrorResult> errors) {
    detections.put(page.getTitle(), new Detection(page, errors));
  }

  /**
   * @param time Time spent.
   */
  public void addTimeSpent(long time) {
    timeSpent += time;
  }

  /**
   * @return Time spent.
   */
  public long getTimeSpent() {
    return timeSpent;
  }
}
