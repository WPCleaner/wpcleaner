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

import org.jdom2.Document;
import org.jdom2.Element;
import org.jdom2.JDOMException;
import org.jdom2.filter.Filters;
import org.jdom2.input.SAXBuilder;
import org.jdom2.xpath.XPathExpression;
import org.jdom2.xpath.XPathFactory;
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
   * @param isbn ISBN
   * @return EAN prefix for the given ISBN.
   */
  public static Range getEANPrefix(String isbn) {
    return getRange(isbn, eanPrefixes);
  }

  /**
   * @param isbn ISBN
   * @return Registration group for the given ISBN.
   */
  public static Range getRegistrationGroup(String isbn) {
    return getRange(isbn, registrationGroups);
  }

  /**
   * @param isbn ISBN
   * @param ranges List of ranges.
   * @return Range for the given ISBN.
   */
  private static Range getRange(String isbn, List<Range> ranges) {
    if ((isbn == null) || (ranges == null)) {
      return null;
    }
    isbn = PageElementISBN.cleanISBN(isbn);
    if (isbn.length() == 10) {
      isbn = "978" + isbn;
    }
    for (Range range : ranges) {
      String cleanPrefix = range.getCleanPrefix();
      if ((cleanPrefix != null) && (isbn.startsWith(cleanPrefix))) {
        return range;
      }
    }
    return null;
  }

  /**
   * Bean for holding information about an ISBN.
   */
  public static class ISBNInformation {
    
    List<String> texts;

    boolean unknownRange;

    boolean reservedRange;

    ISBNInformation() {
      this.texts = new ArrayList<String>();
    }

    public List<String> getTexts() {
      return texts;
    }

    public boolean isInUnknownRange() {
      return unknownRange;
    }

    public boolean isInReservedRange() {
      return reservedRange;
    }
  }

  /**
   * @param isbn ISBN.
   * @return Information about ISBN.
   */
  public static ISBNInformation getInformation(String isbn) {
    if (isbn == null) {
      return null;
    }

    // Retrieve information about ISBN
    isbn = PageElementISBN.cleanISBN(isbn);
    boolean isbn10 = false;
    if (isbn.length() == 10) {
      isbn = "978" + isbn;
      isbn10 = true;
    }
    Range eanPrefix = getEANPrefix(isbn);
    Range registrationGroup = getRegistrationGroup(isbn);

    // Build information
    ISBNInformation isbnInfo = new ISBNInformation();
    if ((eanPrefix != null) && !isbn10) {
      isbnInfo.texts.add(eanPrefix.getPrefix() + " - " + eanPrefix.getAgency());
    }
    String prefix = null;
    String cleanPrefix = null;
    if (registrationGroup != null) {
      prefix = registrationGroup.getPrefix();
      cleanPrefix = registrationGroup.getCleanPrefix();
      if (isbn10 && (prefix != null) && (prefix.length() > 4)) {
        // Remove "978-"
        prefix = prefix.substring(4);
      }
      if ((prefix != null) && (prefix.length() > 0)) {
        isbnInfo.texts.add(prefix + "-" + registrationGroup.getAgency());
      }
    }

    // Suggest a formatted ISBN
    if ((registrationGroup != null) && (cleanPrefix != null)) {
      String suffix = isbn.substring(cleanPrefix.length());
      Rule rule = registrationGroup.getRule(suffix);
      if (rule == null) {
        isbnInfo.texts.add(GT._T("No range found for ISBN"));
        isbnInfo.unknownRange = true;
      } else {
        int nextLength = rule.getLength();
        if (nextLength > 0) {
          if (suffix.length() > nextLength + 1) {
            String suggestedISBN = prefix + "-" +
                suffix.substring(0, nextLength) + "-" +
                suffix.substring(nextLength, suffix.length() - 1) + "-" +
                suffix.substring(suffix.length() - 1);
            isbnInfo.texts.add(GT._T("Suggested format: {0}", suggestedISBN));
          } else {
            isbnInfo.texts.add(GT._T("ISBN length incoherent with range found"));
          }
        } else {
          isbnInfo.texts.add(GT._T(
              "ISBN is in a reserved range {0}",
              prefix + "-(" + rule.getFrom() + "-" + rule.getTo() + ")"));
          isbnInfo.reservedRange = true;
        }
      }
    } else {
      isbnInfo.texts.add(GT._T("No range found for ISBN"));
      isbnInfo.unknownRange = true;
    }

    return isbnInfo;
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
    XPathExpression<Element> xpa = XPathFactory.instance().compile(xpath, Filters.element());
    List<Element> results = xpa.evaluate(root);
    Iterator<Element> iter = results.iterator();
    while (iter.hasNext()) {
      Element node = iter.next();
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
    XPathExpression<Element> xpa = XPathFactory.instance().compile(
        "./Rules/Rule", Filters.element());
    List<Element> results = xpa.evaluate(node);
    Iterator<Element> iter = results.iterator();
    while (iter.hasNext()) {
      Element ruleNode = iter.next();
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
        if (suffix.compareTo(rule.getFrom()) >= 0) {
          String to = rule.getTo();
          if (suffix.length() > to.length()) {
            suffix = suffix.substring(0, to.length());
          }
          if (suffix.compareTo(to) <= 0) {
            return rule;
          }
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
