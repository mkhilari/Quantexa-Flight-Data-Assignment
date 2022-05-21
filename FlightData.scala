import scala.io.BufferedSource
import java.io.PrintWriter
import java.io.File

case class Flight(flightID : Int, 
from : String, to : String, date : Date)

case class Date(year : Int, month : Int, day : Int)

case class Passenger(passengerID : Int, 
firstName : String, lastName : String)

case class PassengerFlight(passengerID : Int, 
flightID : Int)

object FlightData {

    val passengerFilename = "passengers.csv";
    val flightDataFilename = "flightData.csv";

    val question1Filename = "scalaQuestion1.csv";
    val question2Filename = "scalaQuestion2.csv";
    val question3Filename = "scalaQuestion3.csv";
    val question4Filename = "scalaQuestion4.csv";
    val question5Filename = "scalaQuestion5.csv";

    def main(args : Array[String]) = {

        println("Hello, world");

        val passengers = getPassengers(passengerFilename);
        val flights = getFlights(flightDataFilename);
        val passengerFlights = getPassengerFlights(flightDataFilename);

        val passengerIDNumberOfFlights 
        = getPassengerIDNumberOfFlights(passengerFlights);

        val totalFlightsForEachMonth = getTotalFlightsForEachMonth(
        passengers, flights, question1Filename);

        outputTotalFlightsForEachMonth(totalFlightsForEachMonth, 
        question1Filename);

        val mostFrequentFlyers = getMostFrequentFlyers(
        passengers, flights, passengerIDNumberOfFlights, 100);

        outputMostFrequentFlyers(mostFrequentFlyers, 
        question2Filename);
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

    def getPassengerIDNumberOfFlights(
    passengerFlights : Set[PassengerFlight]) : Map[Int, Int] = {

        // Returns a map of (passengerID, numberOfFlights) pairs 
        // from the given set of passenger flights 

        passengerFlights.groupBy(passengerFlight 
        => passengerFlight.passengerID)
        .mapValues(passengerFlights => passengerFlights.size)
        .toList.map((a, b) => (a -> b)).toMap;
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
    passengerIDNumberOfFlights : Map[Int, Int], 
    k : Int) : List[(Passenger, Int)] = {

        // Returns a list of the k most frequent flyers 
        // sorted by number of flights in descending order 

        passengers.values.map(passenger => (passenger, 
        passengerIDNumberOfFlights.getOrElse(passenger.passengerID, 0)))
        .toList.sortBy((passenger, numberOfFlights) => -numberOfFlights)
        .take(k);
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
}