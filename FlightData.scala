import scala.io.BufferedSource
import java.io.PrintWriter
import java.io.File

case class Flight(flightID : Int = 0, 
from : String = "", to : String = "", date : Date = Date())

case class Date(year : Int = 0, month : Int = 0, day : Int = 0) {

    def toDays() : Int = {

        this.year * 1000 + this.month * 12 * this.day;
    }
}

case class Passenger(passengerID : Int = 0, 
firstName : String = "", lastName : String = "")

case class PassengerFlight(passengerID : Int = 0, 
flightID : Int = 0)

case class PassengerPair(passengerIDA : Int = 0, 
passengerIDB : Int = 0)

object FlightData {

    val passengerFilename = "passengers.csv";
    val flightDataFilename = "flightData.csv";

    val question1Filename = "scalaQuestion1.csv";
    val question2Filename = "scalaQuestion2.csv";
    val question3Filename = "scalaQuestion3.csv";
    val question4Filename = "scalaQuestion4.csv";
    val question5Filename = "scalaQuestion5.csv";

    def main(args : Array[String]) = {

        println(s"Reading $passengerFilename");
        val passengers = getPassengers(passengerFilename);

        println(s"Reading $flightDataFilename");
        val flights = getFlights(flightDataFilename);
        val passengerFlights = getPassengerFlights(flightDataFilename);

        val passengerIDFlights 
        = getPassengerIDFlights(flights, passengerFlights);

        println(s"Solving Question 1");
        val totalFlightsForEachMonth = getTotalFlightsForEachMonth(
        passengers, flights, question1Filename);

        outputTotalFlightsForEachMonth(totalFlightsForEachMonth, 
        question1Filename);

        println(s"Solving Question 2");
        val mostFrequentFlyers = getMostFrequentFlyers(
        passengers, flights, passengerIDFlights, 100);

        outputMostFrequentFlyers(mostFrequentFlyers, 
        question2Filename);

        println(s"Solving Question 3");
        val passengerRuns = getPassengerRuns(passengers, flights, 
        passengerIDFlights, "uk");

        outputPassengerRuns(passengerRuns, question3Filename);

        println(s"Solving Question 4");
        val passengerPairs = getPassengerPairs(passengers);
        val passengerPairFlights = getPassengerPairFlights(
        passengerPairs, passengerIDFlights);
        val passengerPairNumberOfFlights 
        = getPassengerPairNumberOfFlights(passengerPairFlights, 3);

        outputPassengerPairNumberOfFlights(passengerPairNumberOfFlights, 
        question4Filename);
    }

    def getPassengers(filename : String) : Map[Int, Passenger] = {

        // Returns a map of (passengerID, passenger) pairs 
        // from the given input filename 

        val inputFile : BufferedSource = io.Source.fromFile(filename);
        
        inputFile.getLines.drop(1).map({
            case s"$passengerID,$firstName,$lastName" 
                => (passengerID.toInt 
                -> Passenger(passengerID.toInt, firstName, lastName))
        }).toMap;
    }

    def getFlights(filename : String) : Map[Int, Flight] = {

        // Returns a map of (flightID, flight) pairs 
        // from the given input filename 

        val inputFile : BufferedSource = io.Source.fromFile(filename);
        
        inputFile.getLines.drop(1).map({
            case s"$passengerID,$flightID,$from,$to,$year-$month-$day" 
                => (flightID.toInt 
                -> Flight(flightID.toInt, from, to, 
                Date(year.toInt, month.toInt, day.toInt)))
        }).toMap;
    }

    def getPassengerFlights(filename : String) : Set[PassengerFlight] = {

        // Returns a set of passenger flights 
        // representing each passenger travel 
        // from the given input filename 

        val inputFile : BufferedSource = io.Source.fromFile(filename);
        
        inputFile.getLines.drop(1).map({
            case s"$passengerID,$flightID,$from,$to,$date" 
                => PassengerFlight(passengerID.toInt, flightID.toInt)
        }).toSet;
    }

    def getPassengerIDFlights(
    flights : Map[Int, Flight], 
    passengerFlights : Set[PassengerFlight]) : Map[Int, List[Int]] = {

        // Returns a map of (passengerID, list of flightIDs) pairs 
        // from the given set of passenger flights 
        // The list of flights is sorted by date in ascending order 

        passengerFlights.groupBy(passengerFlight 
        => passengerFlight.passengerID)
        .mapValues(passengerFlights => passengerFlights.toList.map(
            passengerFlight => passengerFlight.flightID
        ).sortBy(flightID => flights.getOrElse(flightID, Flight())
        .date.toDays())
        ).toMap
    }

    def getPassengerPairs(
    passengers : Map[Int, Passenger]) : Set[PassengerPair] = {

        // Returns a set of passenger pairs 
        // given a set of passengerIDs 

        // Ensures passengerIDA < passengerIDB to avoid 
        // double counting pairs, as well as avoiding 
        // pairs composed of of the same passenger 
        passengers.keySet.flatMap(passengerIDA 
        => passengers.keySet.map(passengerIDB 
        => PassengerPair(passengerIDA, passengerIDB)))
        .filter(passengerPair 
        => passengerPair.passengerIDA < passengerPair.passengerIDB)
    }

    def getPassengerPairFlights(
    passengerPairs : Set[PassengerPair], 
    passengerIDFlights : Map[Int, List[Int]]) 
    : Map[PassengerPair, Set[Int]] = {

        // Returns a map of (passengerPair, 
        // set of flights shared by the passengerPair) 
        // given a set of passenger pairs 
        // and the flights travelled on by each passenger 

        // Create sets of flights rather than lists for more 
        // time efficient O(1) contains methods 
        val passengerIDFlightSet = passengerIDFlights
        .mapValues(flights => flights.toSet)

        passengerPairs.map(passengerPair 
        => (passengerPair 
            -> passengerIDFlightSet.getOrElse(passengerPair
            .passengerIDA, Set())
            .union(passengerIDFlightSet.getOrElse(passengerPair
            .passengerIDB, Set()))
        )).toMap
    }

    def getTotalFlightsForEachMonth(
    passengers : Map[Int, Passenger], 
    flights : Map[Int, Flight], 
    filename : String) : List[(Int, Int)] = {

        flights.values.groupBy(flight => flight.date.month)
        .mapValues(group => group.size).toList
        .sortBy((month, numberOfFlights) => month);
    }

    def outputTotalFlightsForEachMonth(
    totalFlightsForEachMonth : List[(Int, Int)], 
    filename : String) = {

        // Outputs the total flights for each month 
        // to the given output filename 

        val writer = new PrintWriter(new File(filename));

        writer.println("month,numberOfFlights");
        totalFlightsForEachMonth.map((month, numberOfFlights) 
        => s"$month,$numberOfFlights").foreach({
            line => writer.println(line);
        })

        writer.close();
    }

    def getMostFrequentFlyers(
    passengers : Map[Int, Passenger], 
    flights : Map[Int, Flight], 
    passengerIDFlights : Map[Int, List[Int]], 
    numberOfFlyers : Int) : List[(Passenger, Int)] = {

        // Returns a list of the numberOfFlyers 
        // most frequent flyers 
        // sorted by number of flights in descending order 

        passengers.values.map(passenger => (passenger, 
        passengerIDFlights.mapValues(flights => flights.size)
        .getOrElse(passenger.passengerID, 0)))
        .toList.sortBy((passenger, numberOfFlights) => -numberOfFlights)
        .take(numberOfFlyers);
    }

    def outputMostFrequentFlyers(
    mostFrequentFlyers : List[(Passenger, Int)], filename : String) = {

        // Outputs the k most frequent flyers 
        // sorted by number of flights in descending order 
        // to the given output filename 

        val writer = new PrintWriter(new File(filename));

        writer.println("passengerId,numberOfFlights,firstName,lastName");
        mostFrequentFlyers.map((passenger, numberOfFlights) 
        => (passenger.passengerID, numberOfFlights, 
        passenger.firstName, passenger.lastName))
        .map((passengerID, numberOfFlights, firstName, lastName) 
        => s"$passengerID,$numberOfFlights,$firstName,$lastName")
        .foreach({
            line => writer.println(line);
        })

        writer.close();
    }

    def getPassengerRuns(
    passengers : Map[Int, Passenger], 
    flights : Map[Int, Flight], 
    passengerIDFlights : Map[Int, List[Int]], 
    runEnd : String) : List[(Int, Int)] = {

        // Returns the longest runs for each passenger 
        // sorted by longest run in descending order 
        // given a location runEnd 

        // Ensure the start location is included with the first 
        // from location, followed by all to locations 
        passengerIDFlights.mapValues(
        flightIDs 
        => flightIDs.map(
            flightID => flights.getOrElse(flightID, Flight()).from)
            .take(1).concat(
            flightIDs.map(
            flightID => flights.getOrElse(flightID, Flight()).to))
        .reduce((a, b) => s" $a $b ").split(s" $runEnd ")
        .map(run => run.split("\\s+")
        .filter(location => location.size > 0).size)
        .reduce((a, b) => Math.max(a, b)))
        .toList.sortBy((passengerID, longestRun) => -longestRun)
    }

    def outputPassengerRuns(
    passengerRuns : List[(Int, Int)], filename : String) = {

        // Outputs the longest runs for each passenger 
        // sorted by longest run in descending order 
        // to the given output filename 

        val writer = new PrintWriter(new File(filename));

        writer.println("passengerId,longestRun");
        passengerRuns.map((passengerID, longestRun) 
        => s"$passengerID,$longestRun")
        .foreach({
            line => writer.println(line);
        })

        writer.close();
    }

    def getPassengerPairNumberOfFlights(
    passengerPairFlights : Map[PassengerPair, Set[Int]], 
    minSharedFlights : Int) : List[(Int, Int, Int)] = {

        // Returns the number of flights shared by 
        // each passenger pair only if > and not = minSharedFlights 
        // sorted by number of flights shared in descending order 

        passengerPairFlights.toList
        .map((passengerPair, flights) 
        => (passengerPair.passengerIDA, 
        passengerPair.passengerIDB, flights.size))
        .filter((a, b, numberOfFlights) 
        => (numberOfFlights > minSharedFlights))
        .sortBy((a, b, numberOfFlights) => numberOfFlights)
    }

    def outputPassengerPairNumberOfFlights(
    passengerPairNumberOfFlights : List[(Int, Int, Int)], 
    filename : String) = {

        // Outputs the number of flights shared by 
        // each passenger pair 
        // sorted by number of flights shared in descending order 
        // to the given output filename 

        val writer = new PrintWriter(new File(filename));

        writer.println("passenger1Id,passenger2Id,numberOfFlightsTogether");
        passengerPairNumberOfFlights.map(
        (passenger1ID, passenger2Id, numberOfFlights) 
        => s"$passenger1ID,$passenger2Id,$numberOfFlights")
        .foreach({
            line => writer.println(line);
        })

        writer.close();
    }
}