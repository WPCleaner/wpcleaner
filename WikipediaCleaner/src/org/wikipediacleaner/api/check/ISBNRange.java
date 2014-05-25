/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2014  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.check;

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.jdom.Document;
import org.jdom.Element;
import org.jdom.JDOMException;
import org.jdom.input.SAXBuilder;
import org.jdom.xpath.XPath;


/**
 * Utility class to manage ISBN ranges.
 * 
 * Values are extracted from RangeMessage.xml.
 * This file can be generated at https://www.isbn-international.org/range_file_generation.
 */
public class ISBNRange {

  /** Global flag for knowing when ranges are loaded */
  private static boolean rangesLoaded = false;

  /** Lock for loading ranges */
  private static final Object rangesLock = new Object();

  /** EAN prefixes */
  private static List<Range> eanPrefixes = null;

  /** Registration groups */
  private static List<Range> registrationGroups = null;

  /**
   * Utility class initialization.
   */
  public static void initialize() {
    loadRanges();
  }

  /**
   * Load ISBN ranges
   */
  private static void loadRanges() {
    if (rangesLoaded == true) {
      return;
    }
    synchronized (rangesLock) {
      if (rangesLoaded == true) {
        return;
      }
      InputStream is = ISBNRange.class.getClassLoader().getResourceAsStream(
          "org/wikipediacleaner/api/check/RangeMessage.xml");
      if (is != null) {
        analyzeRangeMessage(is);
      }
      rangesLoaded = true;
    }
  }

  /**
   * Analyze RangeMessage.xml file.
   * 
   * @param is Contents of RangeMessage.xml file.
   */
  private static void analyzeRangeMessage(InputStream is) {
    try {
      SAXBuilder sxb = new SAXBuilder();
      Document document = sxb.build(is);
      Element root = document.getRootElement();
      if (root == null) {
        return;
      }
      analyzeEANPrefixes(root);
      analyzeRegistrationGroups(root);
    } catch (IOException e) {
      // Nothing to do
    } catch (JDOMException e) {
      // Nothing to do
    }
  }

  /**
   * Analyze RangeMessage.xml file for EAN Prefixes.
   * 
   * @param root Root of RangeMessage.xml file.
   * @throws JDOMException
   */
  private static void analyzeEANPrefixes(Element root) throws JDOMException {
    eanPrefixes = new ArrayList<Range>();
    analyzeRanges(root, eanPrefixes, "/ISBNRangeMessage/EAN.UCCPrefixes/EAN.UCC");
  }

  /**
   * Analyze RangeMessage.xml file for Registration Groups.
   * 
   * @param root Root of RangeMessage.xml file.
   * @throws JDOMException
   */
  private static void analyzeRegistrationGroups(Element root) throws JDOMException {
    registrationGroups = new ArrayList<Range>();
    analyzeRanges(root, registrationGroups, "/ISBNRangeMessage/RegistrationGroups/Group");
  }

  /**
   * Analyze RangeMessage.xml file for Ranges.
   * 
   * @param root Root of RangeMessage.xml file.
   * @param ranges Current list of ranges.
   * @param xpath XPath selector.
   * @throws JDOMException
   */
  private static void analyzeRanges(Element root, List<Range> ranges, String xpath) throws JDOMException {
    XPath xpa = XPath.newInstance(xpath);
    List results = xpa.selectNodes(root);
    Iterator iter = results.iterator();
    while (iter.hasNext()) {
      Element node = (Element) iter.next();
      Element prefixNode = node.getChild("Prefix");
      String prefix = (prefixNode != null) ? prefixNode.getValue() : null;
      Element agencyNode = node.getChild("Agency");
      String agency = (agencyNode != null) ? agencyNode.getValue() : null;
      Range range = new Range(prefix, agency);
      analyzeRules(node, range);
      ranges.add(range);
    }
  }

  /**
   * Analyze RangeMessage.xml file Rules.
   * 
   * @param node Current node.
   * @param rangeElement Range element.
   * @throws JDOMException
   */
  private static void analyzeRules(Element node, Range rangeElement) throws JDOMException {
    XPath xpa = XPath.newInstance("./Rules/Rule");
    List results = xpa.selectNodes(node);
    Iterator iter = results.iterator();
    while (iter.hasNext()) {
      Element ruleNode = (Element) iter.next();
      Element rangeNode = ruleNode.getChild("Range");
      String range = (rangeNode != null) ? rangeNode.getValue() : null;
      Element lengthNode = ruleNode.getChild("Length");
      String length = (lengthNode != null) ? lengthNode.getValue() : null;
      if ((range != null) && (length != null)) {
        String[] rangeElements = range.split("\\-");
        if ((rangeElements != null) && (rangeElements.length == 2)) {
          Rule rule = new Rule(rangeElements[0], rangeElements[1], Integer.parseInt(length));
          rangeElement.addRule(rule);
        }
      }
    }
  }

  /**
   * Bean for memorizing information about ranges.
   */
  public static class Range {

    /** ISBN prefix */
    private final String prefix;

    /** Agency */
    private final String agency;

    /** Rules */
    private final List<Rule> rules;

    /**
     * @param prefix ISBN prefix.
     * @param agency Agency.
     */
    Range(String prefix, String agency) {
      this.prefix = prefix;
      this.agency = agency;
      this.rules = new ArrayList<ISBNRange.Rule>();
    }

    /**
     * @return ISBN prefix.
     */
    public String getPrefix() {
      return prefix;
    }

    /**
     * @return Agency.
     */
    public String getAgency() {
      return agency;
    }

    /**
     * Add a rule.
     * 
     * @param rule Rule.
     */
    void addRule(Rule rule) {
      rules.add(rule);
    }

    /**
     * @return Description of the EAN Prefix.
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
      StringBuilder buffer = new StringBuilder();
      buffer.append(prefix);
      buffer.append(" - ");
      buffer.append(agency);
      for (Rule rule : rules) {
        buffer.append("\n  ");
        buffer.append(rule);
      }
      return buffer.toString();
    }
  }

  /**
   * Bean for memorizing information about rules.
   */
  public static class Rule {

    /** Beginning of the range */
    private final String from;

    /** End of the range */
    private final String to;

    /** Length of the next element */
    private final int length;

    Rule(String from, String to, int length) {
      this.from = from;
      this.to = to;
      this.length = length;
    }

    /**
     * @return Beginning of the range.
     */
    public String getFrom() {
      return from;
    }

    /**
     * @return End of the range.
     */
    public String getTo() {
      return to;
    }

    /**
     * @return Length of the next element.
     */
    public int getLength() {
      return length;
    }

    /**
     * @return Description of the EAN Prefix.
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
      StringBuilder buffer = new StringBuilder();
      buffer.append(from);
      buffer.append(" - ");
      buffer.append(to);
      buffer.append(" - ");
      buffer.append(length);
      return buffer.toString();
    }
  }
}
