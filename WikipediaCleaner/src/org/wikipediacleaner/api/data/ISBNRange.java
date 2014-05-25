/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2014  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.data;

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
import org.wikipediacleaner.i18n.GT;


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
   * @param isbn ISBN.
   * @return Information about ISBN.
   */
  public static List<String> getInformation(String isbn) {
    if (isbn == null) {
      return null;
    }
    isbn = PageElementISBN.cleanISBN(isbn);
    if (isbn.length() == 10) {
      isbn = "978" + isbn;
    }
    List<String> results = new ArrayList<String>();
    addInformation(isbn, results, eanPrefixes);
    Range range = addInformation(isbn, results, registrationGroups);

    // Suggest a formatted ISBN
    String prefix = (range != null) ? range.getPrefix() : "";
    String cleanPrefix = (range != null) ? range.getCleanPrefix() : "";
    String suffix = isbn.substring(cleanPrefix.length());
    Rule rule = (range != null) ? range.getRule(suffix) : null;
    int nextLength = (rule != null) ? rule.getLength() : 0;

    if (nextLength > 0) {
      if (suffix.length() > nextLength + 1) {
        results.add(
            prefix + "-" +
            suffix.substring(0, nextLength) + "-" +
            suffix.substring(nextLength, suffix.length() - 1) + "-" +
            suffix.substring(suffix.length() - 1));
      } else {
        results.add(GT._("ISBN length incoherent with range found"));
      }
    } else if (rule != null) {
      results.add(GT._(
          "ISBN is in a reserved range {0}",
          prefix + "-(" + rule.getFrom() + "-" + rule.getTo() + ")"));
    } else {
      results.add(GT._("No range found for ISBN"));
    }

    return results;
  }

  /**
   * @param isbn ISBN.
   * @param results Information about ISBN to be completed.
   * @param ranges List of ranges.
   * @return Range according to the ISBN.
   */
  private static Range addInformation(
      String isbn, List<String> results,
      List<Range> ranges) {
    if (ranges == null) {
      return null;
    }
    for (Range range : ranges) {
      String cleanPrefix = range.getCleanPrefix();
      if ((cleanPrefix != null) && (isbn.startsWith(cleanPrefix))) {
        results.add(range.getPrefix() + " - " + range.getAgency());
        return range;
      }
    }
    return null;
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
          "org/wikipediacleaner/api/data/RangeMessage.xml");
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

    /** ISBN prefix */
    private final String cleanPrefix;

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
      this.cleanPrefix = (prefix != null) ? prefix.replaceAll("\\-", "") : null;
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
     * @return ISBN clean prefix.
     */
    public String getCleanPrefix() {
      return cleanPrefix;
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
     * @param suffix Suffix.
     * @return Rule for the next element according to the suffix.
     */
    Rule getRule(String suffix) {
      for (Rule rule : rules) {
        if ((suffix.compareTo(rule.getFrom()) >= 0) &&
            (suffix.compareTo(rule.getTo()) <= 0)) {
          return rule;
        }
      }
      return null;
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
