
import java.io.File;
import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.PriorityQueue;
import java.util.Scanner;
import java.util.Set;

class Flight implements Comparable<Flight> {

    public int flightID;
    public String from;
    public String to;
    public Date date;

    public Set<Passenger> passengers = new HashSet<>();

    public Flight(int flightID, 
    String from, String to, Date date) {

        this.flightID = flightID;
        this.from = from;
        this.to = to;
        this.date = date;
    }

    public int compareTo(Flight other) {

        // Sort flights by date ascending 
        return this.date.compareTo(other.date);
    }

    public boolean equals(Flight other) {

        return (this.flightID == other.flightID);
    }
}

class Date implements Comparable<Date> {

    public int year;
    public int month;
    public int day;

    public Date(int year, int month, int day) {

        this.year = year;
        this.month = month;
        this.day = day;
    }

    public int compareTo(Date other) {

        // Sort by year then month then day ascending 
        if (this.year != other.year) {

            return (this.year - other.year);
        }

        if (this.month != other.month) {

            return (this.month - other.month);
        }

        return (this.day - other.day);
    }
}

class Passenger implements Comparable<Passenger> {

    public int passengerID;
    public String firstName;
    public String lastName;

    public Set<Flight> flights = new HashSet<>();

    public Passenger(int passengerID, 
    String firstName, String lastName) {

        this.passengerID = passengerID;
        this.firstName = firstName;
        this.lastName = lastName;
    }

    public int compareTo(Passenger other) {

        // Sort passengers by number of flights ascending 
        return (this.flights.size() - other.flights.size());
    }
}

class PassengerRun implements Comparable<PassengerRun> {

    public int passengerID;
    public int longestRun;

    public PassengerRun(int passengerID, int longestRun) {

        this.passengerID = passengerID;
        this.longestRun = longestRun;
    }

    public int compareTo(PassengerRun other) {

        // Sorts passenger runs by longest run ascending 
        return (this.longestRun - other.longestRun);
    }
}

class PassengerPair implements Comparable<PassengerPair> {

    public int passengerIDA;
    public int passengerIDB;
    public int numberOfFlights;

    public PassengerPair(int passengerIDA, int passengerIDB, 
    int numberOfFlights) {

        this.passengerIDA = passengerIDA;
        this.passengerIDB = passengerIDB;
        this.numberOfFlights = numberOfFlights;
    }

    public int compareTo(PassengerPair other) {

        // Sorts passenger pairs by number of flights shared ascending 
        return (this.numberOfFlights - other.numberOfFlights);
    }
}

public class FlightData {

    private static String passengerFilename = "passengers.csv";
    private static String flightDataFilename = "flightData.csv";
    private static String question1Filename = "javaQuestion1.csv";
    private static String question2Filename = "javaQuestion2.csv";
    private static String question3Filename = "javaQuestion3.csv";
    private static String question4Filename = "javaQuestion4.csv";
    private static String question5Filename = "javaQuestion5.csv";

    public static void main(String[] args) {
        
        Map<Integer, Passenger> passengers = getPassengers(passengerFilename);
        Map<Integer, Flight> flights = getFlights(flightDataFilename);

        getPassengersOfEachFlight(passengers, flights, flightDataFilename);

        getTotalFlightsForEachMonth(passengers, flights, question1Filename);
        getMostFrequentFlyers(passengers, flights, 100, question2Filename);
        getPassengerRuns(passengers, flights, "uk", question3Filename);
        getPassengersWithSharedFlights(passengers, flights, 3, question4Filename);
    }

    public static Map<Integer, Passenger> getPassengers(String filename) {

        // Returns a map of (passengerID, passenger) pairs 
        // from the given input filename 

        Map<Integer, Passenger> passengers = new HashMap<>();

        try {
            
            Scanner scanner = new Scanner(new File(filename));

            // Read header 
            scanner.nextLine();

            while (scanner.hasNextLine()) {

                // Read passenger line 
                String[] passenger = scanner.nextLine().split(",");

                int passengerID = Integer.parseInt(passenger[0]);
                String firstName = passenger[1];
                String lastName = passenger[2];
                
                passengers.put(passengerID, 
                new Passenger(passengerID, firstName, lastName));
            }
        } catch (FileNotFoundException ex) {

            System.out.println(ex.getMessage());
        }

        return passengers;
    }

    public static Map<Integer, Flight> getFlights(String filename) {

        // Returns a map of (flightID, flight) pairs 
        // from the given input filename 

        Map<Integer, Flight> flights = new HashMap<>();

        try {
            
            Scanner scanner = new Scanner(new File(filename));

            // Read header 
            scanner.nextLine();

            while (scanner.hasNextLine()) {

                // Read flight line 
                String[] flight = scanner.nextLine().split(",");

                int passengerID = Integer.parseInt(flight[0]);
                int flightID = Integer.parseInt(flight[1]);
                String from = flight[2];
                String to = flight[3];
                String[] date = flight[4].split("-");
                int year = Integer.parseInt(date[0]);
                int month = Integer.parseInt(date[1]);
                int day = Integer.parseInt(date[2]);
                
                flights.put(flightID, 
                new Flight(flightID, from, to, 
                new Date(year, month, day)));
            }

            scanner.close();
        } catch (FileNotFoundException ex) {

            System.out.println(ex.getMessage());
        }

        return flights;
    }

    public static void getPassengersOfEachFlight(
    Map<Integer, Passenger> passengers, 
    Map<Integer, Flight> flights, String filename) {

        // Creates a set of passengers that travelled on 
        // each flight, as well as a set of flights that 
        // each passenger travelled on 

        try {
        
            Scanner scanner = new Scanner(new File(filename));

            // Read header 
            scanner.nextLine();

            while (scanner.hasNextLine()) {

                // Read flight line 
                String[] flight = scanner.nextLine().split(",");

                int passengerID = Integer.parseInt(flight[0]);
                int flightID = Integer.parseInt(flight[1]);
                
                // Update the relevant passenger 
                passengers.get(passengerID).flights
                .add(flights.get(flightID));

                // Update the relevant flight 
                flights.get(flightID).passengers
                .add(passengers.get(passengerID));
            }

            scanner.close();
        } catch (FileNotFoundException ex) {

            System.out.println(ex.getMessage());
        }
    }

    public static void getTotalFlightsForEachMonth(
    Map<Integer, Passenger> passengers, 
    Map<Integer, Flight> flights, String filename) {

        // Outputs the total flights for each month 
        // to the given output filename 

        int[] monthNumberOfFlights = new int[12];

        for (Integer flightID : flights.keySet()) {

            int month = flights.get(flightID).date.month;

            monthNumberOfFlights[month - 1]++;
        }

        try {
            PrintWriter writer = new PrintWriter(new File(filename));

            String header = "month,numberOfFlights";
            writer.println(header);

            for (int i = 0; i < monthNumberOfFlights.length; i++) {

                // Write line 
                int month = i + 1;
                String line = month + "," + monthNumberOfFlights[i];
                writer.println(line);
            }

            writer.close();
        } catch (FileNotFoundException ex) {

            System.out.println(ex.getMessage());
        }
    }

    public static void getMostFrequentFlyers(
    Map<Integer, Passenger> passengers, 
    Map<Integer, Flight> flights, int k, String filename) {

        // Outputs the k most frequent flyers 
        // to the given output filename 

        // Create min heap of at most k elements 
        PriorityQueue<Passenger> bestPassengers = new PriorityQueue<>();

        for (Passenger passenger : passengers.values()) {

            bestPassengers.add(passenger);

            // Ensure min heap has at most k elements 
            if (bestPassengers.size() > k) {

                // Remove the (k + 1)th most frequent passenger 
                bestPassengers.poll();
            }
        }

        // Extract the passengers in ascending order 
        List<Passenger> outputPassengers = new ArrayList<>();

        while (!bestPassengers.isEmpty()) {

            outputPassengers.add(bestPassengers.poll());
        }

        Collections.reverse(outputPassengers);

        // Output the top k passengers in order of 
        // number of flights descending 
        try {
            PrintWriter writer = new PrintWriter(new File(filename));

            String header = "passengerId,numberOfFlights," 
            + "firstName,lastName";
            writer.println(header);

            for (Passenger passenger : outputPassengers) {

                // Write line 
                String line = passenger.passengerID + ","
                + passenger.flights.size() + ","
                + passenger.firstName + ","
                + passenger.lastName;
                writer.println(line);
            }

            writer.close();
        } catch (FileNotFoundException ex) {

            System.out.println(ex.getMessage());
        }
    }

    public static void getPassengerRuns(
    Map<Integer, Passenger> passengers, 
    Map<Integer, Flight> flights, String runEnd, String filename) {

        // Outputs the longest runs for each passenger 
        // given a location runEnd 
        // to the given output filename 

        List<PassengerRun> passengerRuns = new ArrayList<>();

        for (int passengerID : passengers.keySet()) {

            // Create a list of flights sortable by date 
            List<Flight> passengerFlights = 
            new ArrayList<>(passengers.get(passengerID).flights);

            int longestRun = getLongestRun(passengerFlights, runEnd);

            passengerRuns.add(new PassengerRun(passengerID, longestRun));
        }

        // Sort passenger runs by longest run descending 
        Collections.sort(passengerRuns);
        Collections.reverse(passengerRuns);

        // Output passenger runs 
        try {
            PrintWriter writer = new PrintWriter(new File(filename));

            String header = "passengerId,longestRun";
            writer.println(header);

            for (PassengerRun passengerRun : passengerRuns) {

                // Write line 
                String line = passengerRun.passengerID + "," 
                + passengerRun.longestRun;
                writer.println(line);
            }

            writer.close();
        } catch (FileNotFoundException ex) {

            System.out.println(ex.getMessage());
        }
    }

    public static int getLongestRun(List<Flight> passengerFlights, 
    String runEnd) {

        // Returns the longest run 
        // given a list of passenger flights 
        // and a location runEnd 

        // Sort passenger flights by date ascending 
        Collections.sort(passengerFlights);

        int longestRun = 0;
        int currentRun = 0;

        if (!passengerFlights.get(0).from.equals(runEnd)) {

            // Start at first location 
            longestRun++;
            currentRun++;
        }

        for (Flight flight : passengerFlights) {

            if (flight.to.equals(runEnd)) {

                // End of current run 
                currentRun = 0;

                continue;
            }

            // Extend current run 
            currentRun++;
            longestRun = Math.max(longestRun, currentRun);
        }

        return longestRun;
    }

    public static void getPassengersWithSharedFlights(
    Map<Integer, Passenger> passengers, 
    Map<Integer, Flight> flights, int k, String filename) {

        // Outputs the passenger pairs sharing > k flights 
        // to the given output filename 

        // passengerPairIDNumberOfFlights is a set of 
        // (passengerPairID, numberOfFlights) pairs 
        Map<String, Integer> passengerPairIDNumberOfFlights = new HashMap<>();

        for (int passengerIDA : passengers.keySet()) {

            for (int passengerIDB : passengers.keySet()) {

                if (passengerIDA >= passengerIDB) {

                    // Same passenger is both parts of the pair 
                    // Ensures passenger A has a lower ID than 
                    // passenger B to avoid double counting 
                    continue;
                }

                // Create the ID representing the pair 
                String passengerPairID = passengerIDA + "&" + passengerIDB;

                // Get number of shared flights 
                int numberOfFlights = 0;

                for (Flight flightA : passengers.get(passengerIDA)
                .flights) {

                    if (passengers.get(passengerIDB).flights
                    .contains(flightA)) {

                        // Current flight is shared 
                        numberOfFlights++;
                    }
                }

                passengerPairIDNumberOfFlights.put(passengerPairID, 
                numberOfFlights);
            }
        }

        List<PassengerPair> passengerPairs = new ArrayList<>();

        for (String passengerPairID : passengerPairIDNumberOfFlights.keySet()) {

            // Create a list of passenger pairs sortable 
            // by number of flights shared 
            String[] passengerIDs = passengerPairID.split("&");
            int passengerIDA = Integer.parseInt(passengerIDs[0]);
            int passengerIDB = Integer.parseInt(passengerIDs[1]);
            int numberOfFlights = passengerPairIDNumberOfFlights
            .get(passengerPairID);

            passengerPairs.add(new PassengerPair(passengerIDA, 
            passengerIDB, numberOfFlights));
        }

        Collections.sort(passengerPairs);
        Collections.reverse(passengerPairs);

        // Output passenger pairs 
        try {
            PrintWriter writer = new PrintWriter(new File(filename));

            String header = "passenger1Id,passenger2Id," 
            + "numberOfFlightsTogether";
            writer.println(header);

            for (PassengerPair passengerPair : passengerPairs) {

                if (passengerPair.numberOfFlights <= k) {

                    // Number of flights shared must be more than k 
                    continue;
                }

                // Write line 
                String line = passengerPair.passengerIDA + "," 
                + passengerPair.passengerIDB + "," 
                + passengerPair.numberOfFlights;
                writer.println(line);
            }

            writer.close();
        } catch (FileNotFoundException ex) {

            System.out.println(ex.getMessage());
        }
    }
}
